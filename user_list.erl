-module(user_list).
-author('Aleksandra Kasznia, Marcin Grzyb').

-export([users/1, getPid/3]).
%This module handles users

users(CurrentState)->
	receive
		{adduser, P, Username} ->
			case addUser(P, Username, CurrentState) of
				{ok, NewState} ->
					P ! {ok, login},
					users(NewState);
				{error, Why} ->
					P ! {error, Why},
					users(CurrentState)
			end;
		{remove, P} ->
			case removeUser(P, CurrentState) of
				{ok, NewState} ->
					P ! {ok, logout},
					users(NewState);
				{error, Why} ->
					P ! {error, Why},
					users(CurrentState)
			end;
		{getpid, Callback, Username} ->
			spawn(user_list, getPid, [Callback, Username, CurrentState]),
			users(CurrentState);
		_ ->
			users(CurrentState)
	end.

addUser(P, Username, CurrentState) ->
	addUser(P, Username, CurrentState, []).

addUser(P, Username, [], Acc) ->
	{ok, lists:reverse([{P, Username} | Acc])};
addUser(P, Username, [Cur | Rest], Acc) ->
	case Cur of
		{_, Username} ->
			{error, "Username in use"};
		_ ->
			addUser(P, Username, Rest, [Cur | Acc])
	end.

removeUser(P, CurrentState) ->
	removeUser(P, CurrentState, []).

removeUser(_, [], _) ->
	{error, "You're not logged in"};
removeUser(P, [Cur | Rest], Acc) ->
	case Cur of
		{P, _} ->
			{ok, lists:append(lists:reverse(Acc), Rest)};
		_ ->
			removeUser(P, Rest, [Cur | Acc])
	end.

getPid(Callback, _, []) ->
	Callback ! {error, "User not logged in"};
getPid(Callback, Username, [Cur | Rest]) ->
	case Cur of
		{P, Username} ->
			Callback ! {ok, P};
		_ ->
			getPid(Callback, Username, Rest)
	end.
