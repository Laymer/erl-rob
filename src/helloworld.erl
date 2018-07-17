% @doc helloworld public API.
% @end
-module(helloworld).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).


-define(PYNODE, 'py@rpi3').
-define(GRISPNODE, 'helloworld@grisp_board').
-define(GRISPPROCESS, grisp_bridge).
-define(PYPROCESS, pyBridge).
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
    net_kernel:connect_node(?PYNODE),
    net_kernel:connect_node(?PYNODE),
    {ok, _} = gen_server:start_link({local, pwmController}, pwmController, [], []),
    {ok, _} = gen_server:start_link({local, motorcontroller}, motorcontroller, [], []),
    {ok, _} = gen_server:start_link({local, motioncontroller}, motioncontroller, [], []),
    {ok, _} = gen_server:start_link({local, pmod_nav2}, pmod_nav2, spi1, []),
    {ok, _} = gen_server:start_link({local, ina219_44}, ina219, 16#44, []), %lipo monitor
    {ok, _} = gen_server:start_link({local, ina219_40}, ina219, 16#40, []), %nimh monitor %disabled until ina@40 gets replaced
    {ok, _} = gen_server:start_link({local, tca9548}, tca9548, 16#70, []),
    {ok, _} = gen_server:start_link({local, distance_server}, distance_server, [], []),
    grisp_gpio:configure_slot(gpio1, {input, input, input, input}),
    %distance_handler:register(), doesn't work, anything that accesses grisp_gpio_events just stalls or doesn't get executed
    spawn(fun pollDistance/0), % so let's do it ourselves. the gpio_events internally uses a timer to poll the pins anyways
    [grisp_led:flash(L, green, 1000) || L <- [1, 2]],
    {ok, Supervisor}.
pollDistance()->
    timer:sleep(250),
    %gen_server:call(motioncontroller, {stop, 4}),
    case grisp_gpio:get(gpio1_1) of
        true -> ok;
        false -> distance_handler:too_close()
    end,
    {?PYPROCESS,?PYNODE} ! {self(), publish, int16, "/platform/e_stop", 1},
    pollDistance().
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
