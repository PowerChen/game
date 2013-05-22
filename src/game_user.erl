%%% 用户进程
-module(game_user).
-behaviour(gen_server).

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{socket,ref,transport}).

start_link(Ref, Socket, Transport, _Opts) ->
	gen_server:start_link(?MODULE,[Ref, Socket, Transport],[]).
	
init([Ref, Socket, Transport]) ->
    {ok, {state, Ref, Socket, Transport}, 0}.
 
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event,State) ->
    {noreply, State}.
	
handle_info(timeout, {state,Ref, Socket, Transport}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
	% 这里初始化用户进程的一些东西
	NewState = #state{socket=Socket,ref=Ref,transport=Transport},
    {noreply, NewState};
	
handle_info({tcp,Socket,Data}, #state{socket=Socket,transport=Transport}=State) ->
	io:format("tcp rcv data=~p ~n",[Data]),
	%% 这里处理socket消息
	ok = Transport:setopts(Socket, [{active, once}]),
	{noreply,State};
	
handle_info({tcp_closed, _Socket}, State) ->
	io:format("tcp closed ~n"),
	{noreply,State}.
	
terminate(_Reason, _State) ->
    ok.

code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.

	