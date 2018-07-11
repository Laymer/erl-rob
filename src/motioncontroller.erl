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

%% API
-export([init/1, handle_call/3, handle_cast/2, stop/0]).
-export([calc_motorSpeeds/2]).

-record(motionState, {
  platform_speed=0.0      :: float(),
  orientation=0.0         :: float(),
  direction=0.0       :: float(),
  status=idle         ::atom()
}).

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
handle_update(orientation, Param, State) ->
  New_state = State#motionState{orientation = Param, status = turning},
  {reply, ok, New_state};
handle_update(speed, Param, State) ->
  New_state = State#motionState{platform_speed = Param},
  {reply, ok, New_state};
handle_update(stop, Param, State) ->
  New_state = State#motionState{platform_speed = 0, status = idle},
  gen_server:call(motorcontroller, {disable, 0}),
  {reply, ok, New_state};
handle_update(go, Param, State) ->
  New_state = State#motionState{status = moving},
  gen_server:call(motorcontroller, {enable, 0}),
  {reply, ok, New_state};
handle_update(apply, Param, State) ->
  M_new = calc_motorSpeeds(State#motionState.direction, State#motionState.platform_speed),
  set_motors(M_new),
  New_state = State#motionState{status = moving},
  {reply, ok, New_state};
handle_update(status, Param, State)->
  {reply, State#motionState.status ,State}.

calc_motorSpeeds(Direction, Speed)  ->
  M1 = Speed * math:sin(Direction*math:pi()/180 + math:pi()/4),
  M2 = Speed * math:cos(Direction*math:pi()/180 + math:pi()/4),
  M3 = Speed * math:cos(Direction*math:pi()/180 + math:pi()/4),
  M4 = Speed * math:sin(Direction*math:pi()/180 + math:pi()/4),
  io:format("speeds: ~p, ~p, ~p, ~p~n", [M1, M2, M3, M4]),
  {M1, M2, M3, M4}.

set_motors({M1, M2, M3, M4})->
  gen_server:call(motorcontroller, {front_left, M1}),
  gen_server:call(motorcontroller, {rear_left, M2}),
  gen_server:call(motorcontroller, {front_right, M3}),
  gen_server:call(motorcontroller, {rear_right, M4}).
