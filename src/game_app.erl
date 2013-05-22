-module(game_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	% 端口监听
	{ok, _} = ranch:start_listener(tcp_game, 4,
		ranch_tcp, [{port, 5555}], game_protocol, []),
    game_sup:start_link().

stop(_State) ->
    ok.
