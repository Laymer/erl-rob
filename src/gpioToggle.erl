-module(gpioToggle).

% Callbacks
 -export([doToggle/0, toggleLoop/0, setup/0, spawnToggle/0]).
% -export([stop/1]).
%
% %--- Callbacks -----------------------------------------------------------------
setup()->
	grisp_gpio:configure_slot(gpio2, {output_0, input, input, input}).
doToggle()->
	grisp_gpio:set(gpio2_1),
	grisp_gpio:clear(gpio2_1).
toggleLoop()->
	doToggle(),
	toggleLoop().
networkSet()->
	receive
		{Remote, set} -> grisp_gpio:set(gpio2_1)
	end,
	Remote ! {self(), clear},
	networkSet().
networkClear()->
	receive
		{Remote, clear} -> grisp_gpio:clear(gpio2_1)
	end,
	Remote ! {self(), set},
	networkClear().
spawnToggle() ->
	Spid = spawn(fun networkSet/0),
	Cpid = spawn(fun networkClear/0),
	Spid ! {Cpid, set}.
	
