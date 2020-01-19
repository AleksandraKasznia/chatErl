-module(room_list).
-author('Aleksandra Kasznia, Marcin Grzyb').

-export([roomList/1, roomState/1, roomSendMsg/2]).
% This module handles chat rooms.


%list of all rooms currently available
roomList(CurrentState) ->
%if the room exists returns room's pid and passes user id to requested room
	receive
		{getpid, P, Room} ->
			case getPid(Room, CurrentState) of
				{ok, Pid} ->
					P ! {ok, Pid},
					roomList(CurrentState);
				{error, createNew} ->
%if the room doesn't exist then create new one and call function again to execute actions described in comment above for the newly created room
					NewPid = spawn(room_list, roomState, [[]]),
					P ! {ok, NewPid},
					roomList([{Room, NewPid} | CurrentState]);
				_ ->
					roomList(CurrentState)
			end;
		{remove, P} ->
			NewList = removeRoom(P, CurrentState),
			roomList(NewList)
	end.

	
getPid(_, []) ->
	{error, createNew};
getPid(Room, [H | Rest]) ->
	case H of
		{Room, Pid} ->
			{ok, Pid};
		_ ->
			getPid(Room, Rest)
	end.
	
	
removeRoom(P, CurrentState) ->
	removeRoom(P, CurrentState, []).

removeRoom(_, [], A) ->
	lists:reverse(A);
	
removeRoom(P, [H | Rest], Acc) ->
	case H of
		{_, P} ->
			lists:append(lists:reverse(Acc), Rest);
		_ ->
			removeRoom(P, Rest, [H | Acc])
	end.

%Each room has its own roomState to keep track of users in the room and broadcast messages
roomState(CurrentState) ->
	receive
		{createNew, P} ->
			roomState([P | CurrentState]);
		{delete, P} ->
			case lists:delete(P, CurrentState) of
				[] ->
%If the last users exits the room; delete the room
					roomlist ! {remove, self()};
				NewState ->
					roomState(NewState)
			end;
		{send, Message} ->
			spawn(room_list, roomSendMsg, [Message, CurrentState]),
			roomState(CurrentState);
		_ ->
			roomState(CurrentState)
	end.

roomSendMsg(_, []) ->
	ok;
roomSendMsg(Message, [H | Rest]) ->
	H ! {send, Message},
	roomSendMsg(Message, Rest).
