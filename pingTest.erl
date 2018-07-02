-module(pingTest).
-export([doPing/0, avgPing/1, doRPC/0, avgRPC/1, doRPC2/0, doRecv/0, avgRecv/1, std_dev/1]).

doPing()->
	S = os:system_time(microsecond), R=net_adm:ping('erl@inspiron'), E = os:system_time(microsecond),{S, E, R, E-S}.

avgPing(0)->{S,E,pong,D} = doPing(), D;
avgPing(N) when N > 0 -> {S,E,pong,D} = doPing(), D+avgPing(N-1).

doRPC() -> S = os:system_time(microsecond), rpc:call('helloworld@grisp_board', grisp_gpio, set, [gpio2_1]), E = os:system_time(microsecond),{S, E, E-S}.
doRPC2() -> S = os:system_time(microsecond), rpc:call('helloworld@grisp_board', grisp_gpio, clear, [gpio2_1]), E = os:system_time(microsecond),{S, E, E-S}.
avgRPC(0)->{_,_,D} = doRPC(), {_,_,D2} = doRPC2(), D+D2;
avgRPC(N) when N > 0 -> {_,_,D} = doRPC(),{_,_,D2} = doRPC2(), D+D2+avgRPC(N-1).

doRecv() ->
	S = os:system_time(microsecond),
	receive
		msg -> E = os:system_time(microsecond)
	end,
	[(E-S)/1000].
avgRecv(0) -> doRecv();
avgRecv(N) when N > 0 ->
	lists:append(doRecv(), avgRecv(N-1)).
std_dev(List) ->
    Average = lists:sum(List) / length(List),
    F = fun(X, Sum) -> Sum + (X - Average) * (X - Average) end,
    Variance = lists:foldl(F, 0.0, List) / length(List),
    math:sqrt(Variance).
