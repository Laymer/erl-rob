%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2018 12:27
%%%-------------------------------------------------------------------
-module(motorcontroller).
-author("Leon Wehmeier").

-behavior(gen_server).
%% API
-export([set_speed/2, setup/0, generateTick/1]).
-export([init/1, handle_call/3, handle_cast/2, stop/0]).

-record(timers, {
  m1            :: timer:tref(),
  m2            :: timer:tref(),
  m3            :: timer:tref(),
  m4            :: timer:tref()
}).
-record(speeds, {
  m1=0            :: non_neg_integer(),
  m2=0            :: non_neg_integer(),
  m3=0            :: non_neg_integer(),
  m4=0            :: non_neg_integer()
}).
-type timers() :: #timers{}.
-type speeds() :: #speeds{}.
-record(state, {
  trefs=#timers{}            :: timers(),
  motor_speeds=#speeds{}            :: speeds(),
  platform_speed=0.0      :: float(),
  orientation=0.0         :: float(),
  direction=0.0       :: float()
}).



init(_Args) ->
  setup(),
  {ok, #state{}}.
stop() ->
  gen_server:cast(motorcontroller, stop).
handle_call({Request, Param}, _From, State) when is_atom(Request) ->
  handle_update(Request, Param, State);
handle_call(_Request, _From, State) ->
  io:format("unknown request"),
  {reply, ok, State}.
handle_cast(stop, State) ->
  {stop, normal, State}.
terminate(normal, State) ->
  ok.

handle_update(set1, Param, State) ->
  io:format("updating motor FL speed"),
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m1 = Param},
  M_trefs = State#state.trefs,
  cancel_timer(M_trefs#timers.m1),
  Intrvl = set_speed(front_left, Param),
  {ok, Tref} = timer:apply_interval(Intrvl, motorcontroller, generateTick, [get_pin_map(front_left)]),
  New_trefs = M_trefs#timers{m1 = Tref},
  New_state = State#state{motor_speeds = New_mspeeds, trefs = New_trefs},
  {reply, ok, New_state}.

cancel_timer(undef) -> {ok};
cancel_timer(Tref) -> timer:cancel(Tref), ok.

set_speed(Motor, 0) -> io:format("set_speed 0 called"), -1;
set_speed(Motor, Speed) when Speed < 0 -> io:format("set_speed negative called"), -Speed;
set_speed(Motor, Speed) when Speed > 0 -> io:format("set_speed fwd called"), Speed.

generateTick(Pin) ->
                    case grisp_gpio:get(Pin) of
                       false -> grisp_gpio:set(Pin);
                       true -> grisp_gpio:clear(Pin)
                    end.

setup() ->
  setup(front_right),
  setup(front_left),
  setup(rear_left),
  setup(rear_right).
setup(Motor) ->
  %grisp_gpio:configure(get_pin_map(Motor), output_0),
  ok.

get_pin_map(front_left) -> led1_r;
get_pin_map(front_right) -> gpio1_2;
get_pin_map(rear_left) -> gpio1_3;
get_pin_map(rear_right) -> gpio1_4.