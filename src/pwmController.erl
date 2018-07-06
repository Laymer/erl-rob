%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2018 15:45
%%%-------------------------------------------------------------------
-module(pwmController).
-author("Leon Wehmeier").

-define(I2C_M_RD, 16#0001).
% Callbacks
-behavior(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
-export([setPWMPeriod/3, setOff/2, setOn/2]).
init(_Args) ->
  process_flag(trap_exit, true),
  {ok, #{init => true, addr=>16#10}}.
handle_call(Call, _From, State) ->
  try execute_call(Call, State)
  catch throw:Reason -> {reply, {error, Reason}, State}
  end.
handle_cast(Request, _State) -> error({unknown_cast, Request}).
handle_info(Info, _State) -> error({unknown_info, Info}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #{slot := Slot}) -> io:format("pwmC termination requested\r\n").

execute_call({set_off, Pin}, State)->
  #{addr:=Addr} = State,
  {reply, setOff(Addr, Pin), State};
execute_call({set_period, Pin, Period_us}, State)->
  #{addr:=Addr} = State,
  {reply, setPWMPeriod(Addr, Pin, Period_us), State};
execute_call({set_off, Pin}, State)->
  #{addr:=Addr} = State,
  {reply, setOff(Addr, Pin), State};
execute_call({set_on, Pin}, State)->
  #{addr:=Addr} = State,
  {reply, setOn(Addr, Pin), State}.


writeReg16(Addr, Reg, Data) ->
  grisp_i2c:msgs([Addr, {write, <<Reg:8, Data:16>>}]).

setPWMPeriod(Addr, Pin, Period_us) ->
  Reg = round(Period_us/10), %we're using 10us steps on the hardware
  <<Swapped:16/little>> = <<Reg:16/big>>,
  writeReg16(Addr, Pin, Swapped). %This sets the MODE1 register to turn on auto increment.
setOff(Addr, Pin)->
  writeReg16(Addr, Pin, 0).
setOn(Addr, Pin) when Pin >= 4 ->
  writeReg16(Addr, Pin, 16#ffff).
