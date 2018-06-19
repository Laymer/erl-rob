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

-define(INA219_REG_CALIBRATION, 16#5).
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

%% API
-export([setup/0, get_voltage/0, get_current/0, get_power/0]).

setup()->
  write_register(?INA219_REG_CALIBRATION, ?INA_CAL_VALUE),
  <<CFGVal:16>> = <<0:2, ?BRNG:1, ?PGA:2, ?BADC:4, ?SADC:4, ?MODE:3>>,
  write_register(?INA219_REG_CONFIG, CFGVal),
  CFGVal.

get_voltage()->
  <<CL:8, CH:8>> = read_register(?INA219_REG_VOLTAGE),
  <<C:13/little-signed-integer, _:3>> = <<CH:8, CL:8>>,
  C*4/1000. %/8000*32

get_current()->
  <<CL:8, CH:8>> = read_register(?INA219_REG_CURRENT),
  <<_:1, C:15/little-signed-integer>> = <<CH:8, CL:8>>,
  C/10.%100uV per mA

get_power()->
  <<CL:8, CH:8>> = read_register(?INA219_REG_POWER),
  <<C:16/little-signed-integer>> = <<CH:8, CL:8>>,
  C*2. %milliwatts

write_register(Reg, Val)->
  <<Valh:8, Vall:8>> = <<Val:16>>,
  grisp_i2c_custom:msgs([16#40, {write, <<Reg:8, Valh:8, Vall:8>>}]).
read_register(Reg)->
  grisp_i2c_custom:msgs([16#40, {write, <<Reg:8>>}, {read, 2, ?I2C_M_RD}]).