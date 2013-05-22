-module(client).
-behaviour(gen_fsm).
-export([start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([connect/2,thinking/2,chat/2]).

-define(BASE_TIME, 5000). 
-define(DELAY, 100).

-record( state,{
	id,
    socket,
	ip,
	port
}).

start_link(Id)->
	% default
	Ip = "192.168.8.119",
	Port = 5555,
    gen_fsm:start_link(?MODULE, [Id,Ip,Port], []).

init([Id,Ip,Port])->
	State = #state{id=Id,ip=Ip,port=Port},
    {ok, connect, State, ?DELAY}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State, ?BASE_TIME}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State, ?BASE_TIME}.

handle_info({tcp,Socket,Data}, StateName, State=#state{socket=Socket}) ->
    io:format("client rev data=~p~n",[Data]),
    {next_state,StateName, State, ?BASE_TIME};
	
handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    % io:format("client tcp_closed~n"),
    {stop, shutdown, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State, ?BASE_TIME}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
	
connect(timeout, #state{ip=Ip,port=Port}=State) ->
    case gen_tcp:connect(Ip, Port, []) of
        {error, Reason} -> {stop, Reason, []};
        {ok, Socket} ->
            {next_state, thinking, State#state{socket=Socket}, ?BASE_TIME} 
    end;
connect(_Event, State) ->
    {stop, {error, unknown_event}, State}.
	
thinking(timeout,State)	->
	% io:format("i am thinking ~n"),
	Act = what_next_do(),
	{next_state,Act,State,?BASE_TIME};
thinking(_Event, State)	->
	{stop,{error,unknown_event},State}.	
	
chat(timeout,#state{socket=Socket}=State)	->
	% io:format("i am chat ~n"),
	Msg = say_something(),
	send_bin(Socket,Msg),
	{next_state,thinking,State,?BASE_TIME};
chat(_Event, State)	->
	{stop,{error,unknown_event},State}.
	

what_next_do()	->
	chat.
	
say_something()	->
	{_,S,_} = now(),
	Rand = S rem 10,
	case Rand of
		1	-> <<"1111">>;
		2	-> <<"2222">>;
		3	-> <<"3333">>;
		4	-> <<"4444">>;
		5	-> <<"5555">>;
		6	-> <<"6666">>;
		7	-> <<"7777">>;
		8	-> <<"8888">>;
		9	-> <<"9999">>;
		0	-> <<"0000">>
	end.
	
send_bin(Socket,Bin)	->
	gen_tcp:send(Socket,Bin).