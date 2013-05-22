%%% 通用工具函数集
-module(game_tool).
-export([second/0]).

%% 当前秒数
second()	-> 
	{M,S,_}  = now(),
	M * 1000000 + S.
