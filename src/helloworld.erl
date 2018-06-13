% @doc helloworld public API.
% @end
-module(helloworld).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    %{ok, Supervisor} = helloworld_sup:start_link(),
    %application:start(grisp),
    %motorcontroller:handle_call({set1, 2500}, [], {state, {timers, undef,undef,undef,undef}, {speeds, 0,0,0,0}, 0.0,0.0,0.0}),
    {ok, Pid} = gen_server:start_link({local, motorcontroller}, motorcontroller, [], []),
    io:format("server started"),
    gen_server:call(motorcontroller, {set1, 500}),
    io:format("set1 called"),
    %gen_server:cast(motorcontroller, stop),
    blink(),
    io:format("blink done").
    %{ok, Supervisor}.

blink() ->
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 250) || L <- LEDs],
    grisp_led:off(2),
    grisp_led:off(1),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
             end,
    grisp_led:pattern(2, [{250, Random}]),
    blink2().
blink2() ->
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
             end,
    grisp_led:pattern(2, [{250, Random}]),
    timer:sleep(15000),
    timer:sleep(15000),
    blink2().
stop(_State) -> ok.
