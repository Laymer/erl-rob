% @doc helloworld public API.
% @end
-module(helloworld).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    {ok, Supervisor} = helloworld_sup:start_link(),
    loop(),
    %application:start(grisp),
    %LEDs = [1, 2],
    %[grisp_led:flash(L, red, 250) || L <- LEDs],
    %grisp_led:off(2),
    %grisp_led:off(1),
    %timer:sleep(1500),
    {ok, _} = gen_server:start_link({local, motorcontroller}, motorcontroller, [], []),
    %gen_server:call(motorcontroller, {set1, 750}),
    %timer:sleep(10000),
    {ok, _} = gen_server:start_link({local, motioncontroller}, motioncontroller, [], []),
    {ok, _} = gen_server:start_link({local, pmod_nav2}, pmod_nav2, spi1, []),
    {ok, _} = gen_server:start_link({local, ina219}, ina219, [], []),
    gen_server:call(motioncontroller, {speed, 1500}),
    gen_server:call(motioncontroller, {direction, 90}),
    gen_server:call(motioncontroller, {apply, dummy}),
    %gen_server:cast(motorcontroller, stop),
    %blink(),
    %io:format("blink done"),
    loop(),
    {ok, Supervisor}.

blink() ->
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 250) || L <- LEDs],
    grisp_led:off(2),
    grisp_led:off(1),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
             end,
    grisp_led:pattern(2, [{250, Random}]).
loop() ->
    timer:sleep(15000),
    loop().
stop(_State) -> ok.
