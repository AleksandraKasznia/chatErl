-module(handleInput).
-author('Aleksandra Kasznia, Marcin Grzyb').

-export([checkInput/1,hasCorrectEnding/1]).
% This module handles user input.


%checks if user ended message correctly - with "\r\n"
hasCorrectEnding(M) ->
	case string:find(M, "\r\n")  of
		nomatch ->
      case string:find(M, "\n") of
        nomatch->
          {ok, M};
        _->
          %deleting "\n" ending
            {ok, string:slice(M, 0, string:length(M)-1)}
      end;
    _->
      %deleting "\r\n" ending
      {ok, string:slice(M, 0, string:length(M)-1)}
	end.

	
%passes list of available commands
checkCommand(C) ->
	checkCommand(C,["LOGIN ", "LOGOUT", "MSG ", "JOIN ", "PART ", "HELP ", "LIST ","BROADCAST","PBRO"]).

checkCommand(_,[]) ->
	{error, "Not a valid command\r\n"};
checkCommand(C,[Cur|Rest]) ->
	case lists:prefix(Cur, C) of
		true ->
			{ok,  lists:subtract(Cur, " "), lists:subtract(C, Cur)};
		false ->
			checkCommand(C, Rest)
	end.

parseCommand(C) ->
	case checkCommand(C) of
		{ok, Command, Data} ->
			case Command of
				"LOGIN" ->
					checkLogin(Data);
				"MSG" ->
					checkMsg(Data);
        "BROADCAST"->
          {ok, message, room,"#BROADCAST", [Data,"\r\n"]};
        "PBRO"->
          {ok,part,"#BROADCAST"};
				"JOIN" ->
					checkJoin(Data);
				"PART" ->
					checkPart(Data);
				"LOGOUT" ->
					checkLogout(Data);
				"LIST" ->
					{ok, list};
				_ ->
					{error, "Not a valid command"}
			end;
		{error, Why} ->
			{error, Why}
	end.

checkInput(M) ->
	case hasCorrectEnding(M) of
		{ok, NewM} ->
			parseCommand(NewM);
		{error, Why} ->
			{error, Why}
	end.

containsSpaceChar(M) ->
%using numerical value of space character
	ContainsSpace = fun(X) -> if X == 32 -> true; true -> false end end,
	lists:any(ContainsSpace, M).

containsHashChar(M) ->
%using numerical value of hash character
	case lists:nth(1, M) of
		35 ->
			true;
		_ ->
			false
	end.

checkLogin([]) ->
	{error, "Please provide username"};
checkLogin(Uname) ->
	case containsSpaceChar(Uname) of
		true ->
			{error, "Spaces are not allowed in a username"};
		false ->
			case containsHashChar(Uname) of
				true ->
					{error, "Username can't start with #"};
				false ->
					{ok, login, Uname}
			end
	end.


checkMsg(Msg) ->
	case checkMsgData(Msg) of
		{ok, To, Txt} ->
			case containsHashChar(Msg) of
				true ->
					{ok, message, room, To, [Txt,"\r\n"]};
				false ->
					{ok, message, user, To, Txt,["\r\n"]}
			end;
		{error, Info, Data} ->
			{error, lists:append([Info, Data])}
	end.


checkMsgData(Msg) ->
	checkMsgData(Msg, []).

checkMsgData([], A) ->
	{error, "Wrong message format", A};
checkMsgData([Cur|Rest], A) ->
	case Cur of
		32 ->
			case A of
				[] ->
					{error, "Please specify destination - provide valid username or room name", Rest};
				[35] ->
					{error, "Room name not specified", Rest};
				_ ->
					{ok, A, Rest}
			end;
		_ ->
			checkMsgData(Rest, lists:append([A, [Cur]]))
	end.

checkJoin([]) ->
	{error, "Please provide room name"};
	
checkJoin(Room) ->
	case containsSpaceChar(Room) of
		true ->
			{error, "Space are not allowed in a room name"};
		false ->
			case containsHashChar(Room) of
				true ->
					{ok, join, Room};
				false ->
					{error, "Room name has to start with #"}
			end
	end.

checkPart(Room) ->
	case containsSpaceChar(Room) of
		true ->
			{error, "Space are not allowed in a room name"};
		false ->
			case containsHashChar(Room) of
				true ->
					{ok, part, Room};
				false ->
					{error, "Room name has to start with #"}
			end
	end.

checkLogout(_) ->
	{ok, logout}.