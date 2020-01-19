-module(mainServer).
-author('Caz').

-export([start/1, stop/0, listen/3, accept/1]).

% This module starts up the chat server and listens for new connections.
% Each new connection is given its own send and receive processes from the
% module titled userHandler.

start(Port) ->
	U = spawn(user_list, users, [[]]),
	R = spawn(room_list, rooms, [[]]),
	register(userlist, U),
	register(roomlist, R),
	register(listener, spawn(mainServer, listen, [Port, U, R])).

stop() ->
	listener ! {quit, self()},
	receive
		{S, U, R} ->
			exit(S, kill),
			exit(U, kill),
			exit(R, kill)
	end.

listen(Port, U, R) ->
	{ok, LSocket} = gen_tcp:listen(Port, [list, {packet, 0}, {active, false}, {reuseaddr, true}]),
	S = spawn(mainServer, accept, [LSocket]),
	register(serv, S),
	receive
		{quit, P} ->
			P ! {S, U, R}
	end.

accept(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			Pid = spawn(userHandler, userState, [Socket]),
			Rx = spawn(userHandler, recvLoop, [Socket, Pid, {0, 0}, 0]),
			Pid ! {rx, Rx},
			accept(LSocket);
		{error, _} ->
			accept(LSocket)
	end.
