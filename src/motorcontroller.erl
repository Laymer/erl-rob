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
-define(MIN_TICK_US, 1000).
-define(MAX_TICK_US, 100000).

-behavior(gen_server).
%% API
-export([set_speed/2, setup/0, enable_motor/1, disable_motor/1, disable_all/0]).
-export([init/1, handle_call/3, handle_cast/2, stop/0, code_change/3]).

-record(speeds, {
  m1=0            :: non_neg_integer(),
  m2=0            :: non_neg_integer(),
  m3=0            :: non_neg_integer(),
  m4=0            :: non_neg_integer()
}).
-type speeds() :: #speeds{}.
-record(state, {
  motor_speeds=#speeds{}            :: speeds()
}).



code_change(_OldVsn, State, _Extra) -> {ok, State}.
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
  set_speed(front_left, 0),
  set_speed(front_right, 0),
  set_speed(rear_right, 0),
  set_speed(rear_left, 0),
  ok.

handle_update(front_left, Param, State) ->
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m1 = Param},
  Intrvl = set_speed(front_left, Param),
  New_state = State#state{motor_speeds = New_mspeeds},
  {reply, ok, New_state};
handle_update(front_right, Param, State) ->
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m2 = Param},
  Intrvl = set_speed(front_right, Param),
  New_state = State#state{motor_speeds = New_mspeeds},
  {reply, ok, New_state};
handle_update(rear_left, Param, State) ->
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m3 = Param},
  Intrvl = set_speed(rear_left, Param),
  New_state = State#state{motor_speeds = New_mspeeds},
  {reply, ok, New_state};
handle_update(rear_right, Param, State) ->
  M_speeds = State#state.motor_speeds,
  New_mspeeds = M_speeds#speeds{m4 = Param},
  Intrvl = set_speed(rear_right, Param),
  New_state = State#state{motor_speeds = New_mspeeds},
  {reply, ok, New_state};
handle_update(enable, Param, State) ->
  enable_all(),
  {reply, ok, State};
handle_update(disable, Param, State) ->
  disable_all(),
  {reply, ok, State}.

set_speed(Motor, 0.0) ->
  set_pwm(Motor, 0),
  ok;
set_speed(Motor, Speed) when Speed < 0 ->
  set_direction(Motor, false),
  Intrvl = -1000000/(800 * Speed), %800 ticks per wheel rotation -> pulse frequency, 1000000us as base time unit
  io:format("setting pwm interval to ~p~n", [Intrvl]),
  set_pwm(Motor, round(Intrvl)); %10us resolution
set_speed(Motor, Speed) when Speed > 0 ->
  set_direction(Motor, true),
  Intrvl = 1000000/(800 * Speed), %800 ticks per wheel rotation -> pulse frequency, 1000000us as base time unit
  io:format("setting pwm interval to ~p~n", [Intrvl]),
  set_pwm(Motor, round(Intrvl)).

set_pwm(Motor, 0) ->
  {PWM, _, _} = get_pin_map(Motor),
  gen_server:call(pwmController, {set_off, PWM});
set_pwm(Motor, Value) when Value > 0, Value < 16#ffff ->
  {PWM, _, _} = get_pin_map(Motor),
  Period = Value,
  gen_server:call(pwmController, {set_period, PWM, Period});
set_pwm(Motor, Value) when Value >= 16#ffff->
  {PWM, _, _} = get_pin_map(Motor),
  io:format("Value ~p exceeds max tick interval, stopping motor ~p", [Value, Motor]),
  gen_server:call(pwmController, {set_period, PWM, 0}).


enable_all() ->
  enable_motor(front_left),
  enable_motor(rear_left),
  enable_motor(front_right),
  enable_motor(rear_right).
disable_all() ->
  disable_motor(front_left),
  disable_motor(rear_left),
  disable_motor(front_right),
  disable_motor(rear_right).
enable_motor(Motor) ->
  {_, Enable, _} = get_pin_map(Motor),
  gen_server:call(pwmController, {set_on, Enable}).
disable_motor(Motor) ->
  {_, Enable, _} = get_pin_map(Motor),
  gen_server:call(pwmController, {set_off, Enable}).

set_direction(front_left, false) ->
  {_, _, Dir} = get_pin_map(front_left),
  gen_server:call(pwmController, {set_on, Dir});
set_direction(front_left, true) ->
  {_, _, Dir} = get_pin_map(front_left),
  gen_server:call(pwmController, {set_off, Dir});
set_direction(rear_left, false) ->
  {_, _, Dir} = get_pin_map(rear_left),
  gen_server:call(pwmController, {set_on, Dir});
set_direction(rear_left, true) ->
  {_, _, Dir} = get_pin_map(rear_left),
  gen_server:call(pwmController, {set_off, Dir});
set_direction(front_right, false) ->
  {_, _, Dir} = get_pin_map(front_right),
  gen_server:call(pwmController, {set_off, Dir});
set_direction(front_right, true) ->
  {_, _, Dir} = get_pin_map(front_right),
  gen_server:call(pwmController, {set_on, Dir});
set_direction(rear_right, false) ->
  {_, _, Dir} = get_pin_map(rear_right),
  gen_server:call(pwmController, {set_off, Dir});
set_direction(rear_right, true) ->
  {_, _, Dir} = get_pin_map(rear_right),
  gen_server:call(pwmController, {set_on, Dir}).

setup() ->
  ok.

% pwm pin, enable pin, dir pin
get_pin_map(rear_left) -> {0, 4, 5};
get_pin_map(front_left) -> {1, 6, 7};
get_pin_map(front_right) -> {2, 8, 9};
get_pin_map(rear_right) -> {3, 10, 11}.
