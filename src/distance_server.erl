%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2018 14:05
%%%-------------------------------------------------------------------
-module(distance_server).
-author("Leon Wehmeier").

% Callbacks
-behavior(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
%% debug
-export([read_sensor/1]).


init(_Args) ->
  process_flag(trap_exit, true),
  {ok, #{init => true}}.
handle_call(Call, _From, State) ->
  try execute_call(Call, State)
  catch throw:Reason -> {reply, {error, Reason}, State}
  end.
handle_cast(Request, _State) -> error({unknown_cast, Request}).
handle_info(Info, _State) -> error({unknown_info, Info}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #{slot := Slot}) -> io:format("ina219 termination requested\r\n").

execute_call({read_distance, Sensor}, State)->
  #{addr:=Addr} = State,
  {reply, read_sensor(Sensor), State}.
startSensor(Sensor)->
  gen_server:call(tca9548, {set_channel, get_sensor_pin(Sensor)}),
  vl6180x:initVL(),
  vl6180x:startContinous().
read_sensor(Sensor)->
  gen_server:call(tca9548, {set_channel, get_sensor_pin(Sensor)}),
  vl6180x:readRange(16#29).

get_sensor_pin(front_left) -> 0;
get_sensor_pin(front_left) -> 5;
get_sensor_pin(right_front) ->1;
get_sensor_pin(right_rear) -> 2;
get_sensor_pin(left_front) -> 3;
get_sensor_pin(left_rear) -> 4.