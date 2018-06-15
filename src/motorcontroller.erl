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
  motor_speeds=#speeds{}            :: speeds()
}).



init(_Args) ->
  setup(),
  {ok, #state{}}.
stop() ->
  gen_server:cast(motorcontroller, stop).
handle_call({Request, Param}, _From, State) when is_atom(Request) ->
  handle_update(Request, Param, State);
handle_call(_Request, _From, State) ->
  io:format("unknown request\r\n"),
  {reply, ok, State}.
handle_cast(stop, State) ->
  {stop, normal, State}.
terminate(normal, State) ->
  cancel_timer(State#state.trefs#timers.m1),
  cancel_timer(State#state.trefs#timers.m2),
  cancel_timer(State#state.trefs#timers.m3),
  cancel_timer(State#state.trefs#timers.m4),
  ok.

%TODO: refactor to use case on motor
handle_update(set1, Param, State) ->
  io:format("updating motor FL speed\r\n"),
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m1 = Param},
  M_trefs = State#state.trefs,
  cancel_timer(M_trefs#timers.m1),
  Intrvl = set_speed(front_left, Param),
  case timer:apply_interval(Intrvl, motorcontroller, generateTick, [get_pin_map(front_left)]) of
    {ok, Tref} -> New_trefs = M_trefs#timers{m1 = Tref};
    {error, badarg} -> New_trefs = M_trefs#timers{m1 = undef}
  end,
  New_state = State#state{motor_speeds = New_mspeeds, trefs = New_trefs},
  {reply, ok, New_state};
handle_update(set2, Param, State) ->
  io:format("updating motor FR speed\r\n"),
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m2 = Param},
  M_trefs = State#state.trefs,
  cancel_timer(M_trefs#timers.m2),
  Intrvl = set_speed(front_right, Param),
  case timer:apply_interval(Intrvl, motorcontroller, generateTick, [get_pin_map(front_right)]) of
    {ok, Tref} -> New_trefs = M_trefs#timers{m2 = Tref};
    {error, badarg} -> New_trefs = M_trefs#timers{m2 = undef}
  end,
  New_state = State#state{motor_speeds = New_mspeeds, trefs = New_trefs},
  {reply, ok, New_state};
handle_update(set3, Param, State) ->
  io:format("updating motor RL speed\r\n"),
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m3 = Param},
  M_trefs = State#state.trefs,
  cancel_timer(M_trefs#timers.m3),
  Intrvl = set_speed(rear_left, Param),
  case timer:apply_interval(Intrvl, motorcontroller, generateTick, [get_pin_map(rear_left)]) of
    {ok, Tref} -> New_trefs = M_trefs#timers{m3 = Tref};
    {error, badarg} -> New_trefs = M_trefs#timers{m3 = undef}
  end,
  New_state = State#state{motor_speeds = New_mspeeds, trefs = New_trefs},
  {reply, ok, New_state};
handle_update(set4, Param, State) ->
  io:format("updating motor RR speed\r\n"),
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m4 = Param},
  M_trefs = State#state.trefs,
  cancel_timer(M_trefs#timers.m4),
  Intrvl = set_speed(rear_right, Param),
  case timer:apply_interval(Intrvl, motorcontroller, generateTick, [get_pin_map(rear_right)]) of
    {ok, Tref} -> New_trefs = M_trefs#timers{m4 = Tref};
    {error, badarg} -> New_trefs = M_trefs#timers{m4 = undef}
  end,
  New_state = State#state{motor_speeds = New_mspeeds, trefs = New_trefs},
  {reply, ok, New_state}.

cancel_timer(undef) -> {ok};
cancel_timer(Tref) -> timer:cancel(Tref), ok.

set_speed(Motor, 0) -> io:format("set_speed 0 called\r\n"), -1;
set_speed(front_right, Speed) when Speed > 0 -> io:format("set_speed fwd fr called\r\n"),
  grisp_gpio:set(led1_b),
  Speed;
set_speed(front_right, Speed) when Speed < 0 -> io:format("set_speed bwd fr called\r\n"),
  grisp_gpio:clear(led1_b),
  -Speed;
set_speed(Motor, Speed) when Speed < 0 -> io:format("set_speed negative called\r\n"), -Speed;
set_speed(Motor, Speed) when Speed > 0 -> io:format("set_speed fwd called\r\n"), Speed.

generateTick(Pin) ->
                    case grisp_gpio:get(Pin) of
                        false -> grisp_gpio:set(Pin);
                       true -> grisp_gpio:clear(Pin)
                    end.

setup() ->
  grisp_gpio:configure(led1_b, output_0),
  setup(front_right),
  setup(front_left),
  setup(rear_left),
  setup(rear_right).
setup(Motor) ->
  grisp_gpio:configure(get_pin_map(Motor), output_0),
  ok.

get_pin_map(front_left) -> led1_r;
get_pin_map(front_right) -> led1_g;
get_pin_map(rear_left) -> gpio1_3;
get_pin_map(rear_right) -> gpio1_4.