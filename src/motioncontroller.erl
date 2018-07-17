%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jun 2018 15:43
%%%-------------------------------------------------------------------
-module(motioncontroller).
-author("Leon Wehmeier").
%TODO: motion overlay
-behavior(gen_server).

-define(WHEEL_SEP_LENGTH, 0.2).
-define(WHEEL_SEP_WIDTH, 0.2).
-define(WHEEL_D, 0.188/2). %30mm*pi
%% API
-export([init/1, handle_call/3, handle_cast/2, stop/0, code_change/3]).
-export([calc_motorSpeeds/3]).

-record(motionState, {
  platform_speed=0.0      :: float(),
  theta=0.0         :: float(),
  direction=0.0       :: float(),
  status=idle         ::atom()
}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.
init(_Args) ->
  io:format("motioncontroller started\r\n"),
  {ok, #motionState{}}.
stop() ->
  gen_server:cast(motioncontroller, stop).
handle_call({Request, Param}, _From, State) when is_atom(Request) ->
  handle_update(Request, Param, State);
handle_call(_Request, _From, State) ->
  io:format("unknown request\r\n"),
  {reply, ok, State}.
handle_cast(stop, State) ->
  {stop, normal, State}.
terminate(normal, State) ->
  stop().

handle_update(direction, Param, State) ->
  New_state = State#motionState{direction = Param},
  {reply, ok, New_state};
handle_update(theta, Param, State) ->
  New_state = State#motionState{theta = Param, status = turning},
  {reply, ok, New_state};
handle_update(speed, Param, State) ->
  New_state = State#motionState{platform_speed = Param},
  {reply, ok, New_state};
handle_update(combined, Param, State) ->
  {Speed, Direction, Theta} = Param,
  New_state = State#motionState{platform_speed = Speed, direction = Direction, theta = Theta, status = moving},
  M_new = calc_motorSpeeds(State#motionState.direction, State#motionState.platform_speed, State#motionState.theta),
  set_motors(M_new),
  {reply, ok, New_state};
handle_update(xyTheta, Param, State) ->
  {X, Y, Theta} = Param,
  M_new = calc_motorSpeeds_xyt(X, Y, Theta),
  set_motors(M_new),
  %gen_server:call(motorcontroller, {enable, 0}),
  {reply, ok, State};
handle_update(stop, Param, State) ->
  New_state = State#motionState{platform_speed = 0, status = idle},
  gen_server:call(motorcontroller, {disable, 0}),
  {reply, ok, New_state};
handle_update(go, Param, State) ->
  New_state = State#motionState{status = moving},
  gen_server:call(motorcontroller, {enable, 0}),
  {reply, ok, New_state};
handle_update(apply, Param, State) ->
  M_new = calc_motorSpeeds(State#motionState.direction, State#motionState.platform_speed, State#motionState.theta),
  set_motors(M_new),
  New_state = State#motionState{status = moving},
  {reply, ok, New_state};
handle_update(status, Param, State)->
  {reply, State#motionState.status ,State}.

calc_motorSpeeds(Direction, Speed, Theta)  ->
  M1 = Speed * math:sin(Direction*math:pi()/180 + math:pi()/4) - Theta,
  M2 = Speed * math:cos(Direction*math:pi()/180 + math:pi()/4) + Theta,
  M3 = Speed * math:cos(Direction*math:pi()/180 + math:pi()/4) - Theta,
  M4 = Speed * math:sin(Direction*math:pi()/180 + math:pi()/4) + Theta,
  io:format("speeds: ~p, ~p, ~p, ~p~n", [M1, M2, M3, M4]),
  {M1, M2, M3, M4}.

calc_motorSpeeds_xyt(X, Y, Theta)  -> %in m/s and rad/s
  M1 = 1/?WHEEL_D*(X-Y-(?WHEEL_SEP_WIDTH + ?WHEEL_SEP_LENGTH)*Theta), % front left
  M2 = 1/?WHEEL_D*(X+Y-(?WHEEL_SEP_WIDTH + ?WHEEL_SEP_LENGTH)*Theta), % rear left
  M3 = 1/?WHEEL_D*(X+Y+(?WHEEL_SEP_WIDTH + ?WHEEL_SEP_LENGTH)*Theta), % front right
  M4 = 1/?WHEEL_D*(X-Y+(?WHEEL_SEP_WIDTH + ?WHEEL_SEP_LENGTH)*Theta), % rear right
  io:format("speeds (rps): ~p, ~p, ~p, ~p~n", [M1, M2, M3, M4]),
  {M1, M2, M3, M4}.

set_motors({M1, M2, M3, M4})->
  gen_server:call(motorcontroller, {front_left, M1}),
  gen_server:call(motorcontroller, {rear_left, M2}),
  gen_server:call(motorcontroller, {front_right, M3}),
  gen_server:call(motorcontroller, {rear_right, M4}).
