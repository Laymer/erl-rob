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
    %sleepForever(),
    %application:start(grisp),
    %LEDs = [1, 2],
    %[grisp_led:flash(L, red, 250) || L <- LEDs],
    %grisp_led:off(2),
    %grisp_led:off(1),
    %timer:sleep(1500),
    {ok, _} = gen_server:start_link({local, pwmController}, pwmController, [], []),
    {ok, _} = gen_server:start_link({local, motorcontroller}, motorcontroller, [], []),
    {ok, _} = gen_server:start_link({local, motioncontroller}, motioncontroller, [], []),
    {ok, _} = gen_server:start_link({local, pmod_nav2}, pmod_nav2, spi1, []),
    {ok, _} = gen_server:start_link({local, ina219_44}, ina219, 16#44, []), %lipo monitor
    {ok, _} = gen_server:start_link({local, ina219_40}, ina219, 16#40, []), %nimh monitor %disabled until ina@40 gets replaced
    {ok, _} = gen_server:start_link({local, tca9548}, tca9548, 16#70, []),
    {ok, _} = gen_server:start_link({local, distance_server}, distance_server, [], []),
    distance_handler:register(),
    %gen_server:cast(motorcontroller, stop),
    %blink(),
    %io:format("blink done"),
    %loop(),
    [grisp_led:flash(L, green, 1000) || L <- [1, 2]],
    {ok, Supervisor}.

sleepForever()->
    timer:sleep(5000),
    sleepForever().
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
