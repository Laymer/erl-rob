%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jul 2018 12:22
%%%-------------------------------------------------------------------
-module(ros_interface).
-author("Leon Wehmeier").

%% API
-export([publish/3, subscribe/2, stop/0, connect/0]).

-define(PYNODE, 'py@rpi3').
-define(GRISPNODE, 'helloworld@grisp_board').
-define(GRISPPROCESS, grisp_bridge).
-define(PYPROCESS, pyBridge).

connect()->
  ConnectedRemoteNode = net_kernel:connect_node(?PYNODE),
  case ConnectedRemoteNode of
    true ->
      Pid = spawn(fun loop/0),
      erlang:register(erlBridge, Pid);
    false ->
      io:format("Could not connect to Python node, is it running? ~n"),
      false
  end.
stop() ->
  {?PYPROCESS,?PYNODE} ! {self(), stop},
  erlBridge ! stop,
  unregister(erlBridge).

subscribe(MsgFormat, Topic) ->
  {?PYPROCESS,?PYNODE} ! {self(), subscribe, MsgFormat, Topic},
  receive
    {_, {ok, Topic}} -> ok;
    {_, {err, already_subscribed}} -> ok;
    {_, {err, unknown_message_type, Type}} -> {err, unknown_message_type, Type};
    Msg -> {err, unknown_response, Msg}
  after 1500 ->
    exit({pyBridge_timeout, subscribe, Topic})
  end.
publish(MsgFormat, Topic, Data) ->
  {?PYPROCESS,?PYNODE} ! {self(), publish, MsgFormat, Topic, Data},
  receive
    {_, {ok, Topic}} -> ok;
    {_, {err, unknown_message_type, Type}} -> {err, unknown_message_type, Type};
    Msg -> {err, unknown_response, Msg}
  after 5500 ->
    exit({pyBridge_timeout, publish, Topic})
  end.
loop()->
  receive
    stop ->
      io:format("Stopping loop~n");
    {Remote, {push, Topic, String}} ->
      io:format("Received from ~p on topic ~s: ~s~n", [Remote, Topic, String]),
      loop()
  end.