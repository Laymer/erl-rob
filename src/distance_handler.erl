%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jul 2018 10:21
%%%-------------------------------------------------------------------
-module(distance_handler).
-author("Leon Wehmeier").
-behavior(gen_event).

-export([register/0]).
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).



%--- API -----------------------------------------------------------------------
register() ->
  io:format("registering...~n"),
  gen_event:add_handler(grisp_gpio_events, ?MODULE, undefined).
%--- Callbacks -----------------------------------------------------------------
init(_Args) ->
  io:format("registered~n"),
  {ok, undefined}.

handle_event({gpio1_1, false}, State) ->
  io:format("e-stop triggered by distance sensor array~n"),
  Pins = [front_left, front_right, left_front, left_rear, right_front, right_rear],
  lists:foreach(fun(Pin) ->
                  D = gen_server:call(distance_server, {read_distance, Pin}),
                  case D of
                    S when S =< 125 -> io:format("Sensor ~p triggered~n", [Pin]);
                    S when S > 125 -> ok
                  end
                end, Pins),
  {ok, State};
handle_event(Event, State) ->
  io:format("ignoring ~p~n", [Event]),
  {ok, State}.

handle_call(Request, _State) -> error({unknown_request, Request}).
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> State.
terminate(_Arg, _State) ->
  undefined.
