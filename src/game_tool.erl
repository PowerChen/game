% ͨ�ù��ߺ�����
-module(game_tool).
-export([second/0]).

%% ��ǰ����
second()	-> 
	{M,S,_}  = now(),
	M * 1000000 + S.
