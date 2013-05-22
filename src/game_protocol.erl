-module(game_protocol).
-export([start_link/4, init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	{OK, Closed, Error} = Transport:messages(),
	Transport:setopts(Socket, [{active, once}]),
	receive
		{OK, Socket, _Data} ->
			% error_logger:info_msg("loop in socket = ~p~n",[_Data]),
			% 这里消息解包处理
			loop(Socket, Transport);
		{Closed, Socket} ->
			error_logger:info_msg("loop in socket closed~n"),
			ok;
		{Error, Socket, _} ->
			error_logger:info_msg("loop in error socket = ~p~n",[Error]),
			ok = Transport:close(Socket)
	end.
