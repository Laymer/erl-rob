%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jun 2018 20:08
%%%-------------------------------------------------------------------
-module(ina219).
-author("Leon Wehmeier").

-define(INA219_ADDR, 16#40).
-define(I2C_M_RD, 16#0001).

-define(INA219_REG_CALIBRATION, 5).
-define(INA219_REG_CONFIG, 0).
-define(INA219_REG_VOLTAGE, 2).
-define(INA219_REG_POWER, 3).
-define(INA219_REG_CURRENT, 4).

-define(PGA, 2#11).%320mv range
-define(BRNG, 2#1). %32V range
-define(BADC, 2#1000).
-define(SADC, 2#1000).%12 bit ADC, 500us conversion time
-define(MODE, 2#111).%continous monitoring

-define(INA_CAL_VALUE, 4096). %magic value, sets to 32V, 2A


% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
%% API
-export([setup/1, get_voltage/1, get_current/1, get_power/1]).


% @private
start_link() -> gen_server:start_link(?MODULE, [], []).


init(I2CAddr) ->
  process_flag(trap_exit, true),
  setup(I2CAddr),
  {ok, #{init => true, addr=> I2CAddr}}.
handle_call(Call, _From, State) ->
  try execute_call(Call, State)
  catch throw:Reason -> {reply, {error, Reason}, State}
  end.
handle_cast(Request, _State) -> error({unknown_cast, Request}).
handle_info(Info, _State) -> error({unknown_info, Info}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #{slot := Slot}) -> io:format("ina219 termination requested\r\n").

execute_call(get_voltage, State)->
  #{addr:=Addr} = State,
  {reply, get_voltage(Addr), State};
execute_call(get_current, State)->
  #{addr:=Addr}=State,
  {reply, get_current(Addr), State};
execute_call(get_power, State)->
  #{addr:= Addr} = State,
  {reply, get_power(Addr), State}.

setup(Addr)->
  write_register(Addr, ?INA219_REG_CALIBRATION, ?INA_CAL_VALUE),
  <<CFGVal:16>> = <<0:2, ?BRNG:1, ?PGA:2, ?BADC:4, ?SADC:4, ?MODE:3>>,
  write_register(Addr, ?INA219_REG_CONFIG, CFGVal),
  CFGVal.

get_voltage(Addr)->
  <<CL:8, CH:8>> = read_register(Addr, ?INA219_REG_VOLTAGE),
  <<C:13/little-signed-integer, _:3>> = <<CH:8, CL:8>>,
  C*4/1000. %/8000*32

get_current(Addr)->
  <<_:1, CL:7, CH:8>> = read_register(Addr, ?INA219_REG_CURRENT),
  <<C:15/little-signed-integer>> = <<CH:8, CL:7>>,
  C/10.%100uV per mA

get_power(Addr)->
  <<CL:8, CH:8>> = read_register(Addr, ?INA219_REG_POWER),
  <<C:16/little-signed-integer>> = <<CH:8, CL:8>>,
  C*2. %milliwatts

write_register(Addr, Reg, Val)->
  <<Valh:8, Vall:8>> = <<Val:16>>,
  grisp_i2c:msgs([Addr, {write, <<Reg:8, Valh:8, Vall:8>>}]).
read_register(Addr, Reg)->
  grisp_i2c:msgs([Addr, {write, <<Reg:8>>}, {read, 2, ?I2C_M_RD}]).
