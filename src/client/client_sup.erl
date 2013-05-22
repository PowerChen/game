-module(client_sup).
-behaviour(supervisor).
-export([init/1,start/0,stop/0]).
-export([add/1,add/2,del/1,stat/0]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

start() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
		{ok,Pid}	->
			unlink(Pid),
			{ok,Pid};
		Other	->
			Other
			
	end.

stop()	->
	case whereis(?MODULE) of
		undefined	->
			skip;
		Pid	->
			exit(Pid,kill)
	end.
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(client,worker)]} }.
	
add(N)	->
	add(1,N).
add(B,E)	->
	[supervisor:start_child(?MODULE,[X])||X<-lists:seq(B,E)],
	ok.
	
del(Num) when Num > 0 ->
    List = [Pid || {_Nick, Pid, worker, [client]} <- supervisor:which_children(?MODULE), is_process_alive(Pid)],
    del(List, Num).
del(_, 0) -> ok;
del([Pid|T], Num) ->
    exit(Pid, shutdown),
    del(T, Num-1);
del([], _) ->
    ok.

stat() ->
    List = supervisor:count_children(?MODULE),
    lists:keyfind(active, 1, List).
	