%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2018 12:49
%%%-------------------------------------------------------------------
-module(pca9685).
-author("Leon Wehmeier").


-define(PCA9685_MODE1, 16#0).
-define(PCA9685_MODE2, 16#01).
-define(PCA9685_PRESCALE, 16#fe).
-define(LED0_ON_L, 16#06).
-define(I2C_M_RD, 16#0001).

% Callbacks
-behavior(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
%% debug
-export([initPCA/1, writeReg8/3, readReg8/2, writeReg16/3, setPin/3, setPWM/3]).


init(I2CAddr) ->
  process_flag(trap_exit, true),
  initPCA(I2CAddr),
  {ok, #{init => true, addr=>I2CAddr}}.
handle_call(Call, _From, State) ->
  try execute_call(Call, State)
  catch throw:Reason -> {reply, {error, Reason}, State}
  end.
handle_cast(Request, _State) -> error({unknown_cast, Request}).
handle_info(Info, _State) -> error({unknown_info, Info}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #{slot := Slot}) -> io:format("ina219 termination requested\r\n").

execute_call({set_pin, Pin, PinState}, State)->
  #{addr:=Addr} = State,
  {reply, setPin(Addr, Pin, PinState), State};
execute_call({set_pwm, Pin, PWMVal}, State)->
  #{addr:=Addr} = State,
  {reply, setPWM(Addr, Pin, PWMVal), State}.


writeReg16(Addr, Reg, Data) ->
  grisp_i2c:msgs([Addr, {write, <<Reg:8, Data:16>>}]).
writeReg32(Addr, Reg, Data) ->
  grisp_i2c:msgs([Addr, {write, <<Reg:8, Data:32>>}]).
writeReg8(Addr, Reg, Data) ->
  grisp_i2c:msgs([Addr, {write, <<Reg:8, Data:8>>}]).
readReg8(Addr, Reg) ->
  grisp_i2c:msgs([Addr, {write, <<Reg:8>>}]),
  grisp_i2c:msgs([Addr, {read, 1, ?I2C_M_RD}]).

setPWMFreq(Addr, Val) ->
  Freq = 0.9 * Val,
  Prescaleval = round(25000000 / 4096 / Freq - 1),
  <<Oldmode:8>> = readReg8(Addr, ?PCA9685_MODE1),
  Newmode = (Oldmode band 16#7F) bor 16#10, % sleep
  writeReg8(Addr, ?PCA9685_MODE1, Newmode),
  writeReg8(Addr,?PCA9685_PRESCALE, Prescaleval),% set the prescaler
  writeReg8(Addr, ?PCA9685_MODE1, Oldmode),
  writeReg8(Addr, ?PCA9685_MODE1, Oldmode bor 16#a0). %This sets the MODE1 register to turn on auto increment.

initPCA(Addr)->
  grisp_i2c:msgs([Addr, {write, <<6:8>>}]),
  writeReg8(Addr, ?PCA9685_MODE1, 16#A0),
  writeReg8(Addr, ?PCA9685_MODE2, 16#0C).

setPin(Addr, Pin, high)->
  <<Val:32>> = <<16#00100000:32>>,
  writeReg32(Addr, ?LED0_ON_L+4*Pin, Val);
setPin(Addr, Pin, low)->
  <<Val:32>> = <<16#00000010:32>>,
  writeReg32(Addr, ?LED0_ON_L+4*Pin, Val).
setPWM(Addr, Pin, PWMVal)->
  <<Val:32>> = <<0:16, PWMVal:16/little>>,
  writeReg32(Addr, ?LED0_ON_L+4*Pin, Val).
