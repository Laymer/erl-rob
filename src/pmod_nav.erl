%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jun 2018 17:46
%%%-------------------------------------------------------------------
-module(pmod_nav).
-author("Leon Wehmeier").
%Note: ACC and Gyro only
-define(ACCX_REGL, 16#28).
-define(ACCX_REGH, 16#29).
-define(ACCY_REGL, 16#2A).
-define(ACCY_REGH, 16#2B).
-define(ACCZ_REGL, 16#2C).
-define(ACCZ_REGH, 16#2D).
-define(GYX_REGL, 16#18).
-define(GYX_REGH, 16#19).
-define(GYY_REGL, 16#1A).
-define(GYY_REGH, 16#1B).
-define(GYZ_REGL, 16#1C).
-define(GYZ_REGH, 16#1D).

-define(CTRL_REG5_XL, 16#1F).
-define(CTRL_REG6_XL, 16#20).
-define(CTRL_REG7_XL, 16#21).
-define(CTRL_REG1_G, 16#10).
-define(CTRL_REG2_G, 16#11).
-define(CTRL_REG3_G, 16#12).
-define(ORIENT_CFG_G, 16#13).
-define(CTRL_REG4, 16#1E).

%Gyro config
-define(ODR_G, 2#010). %data rate 59.5Hz
-define(BW_G, 2#11). %cutoff 16
-define(FS_G, 2#00). %full-scale:245dps
-define(INT_SEL, 2#00).
-define(OUT_SEL, 2#00).
-define(LP_MODE, 2#0).
-define(HP_EN, 2#0).
-define(HPCF_G, 2#0000).
-define(SIGNX_G, 2#0).
-define(SIGNY_G, 2#0).
-define(SIGNZ_G, 2#0).
-define(ORIENT_G, 2#000).
-define(ZEN_G, 2#1).
-define(YEN_G, 2#1).
-define(XEN_G, 2#1).
-define(LIR_XL1, 2#0).
-define(D4_XL1, 2#0).
%Acc config
-define(DEC_XL, 2#11). %update every 8 samples
-define(ZEN_XL, 2#1).
-define(YEN_XL, 2#1).
-define(XEN_XL, 2#1).
-define(ODR_XL, 2#110).%952Hz
-define(FS_XL, 2#00).%2g scale, 11 is 8g, 01 16g, 10 4g
-define(BW_SCAL_ODR, 2#1).%bandwidth by AA filter
-define(BW_XL, 2#00).%408Hz
-define(HR_XL, 2#1). %high res for ACC
-define(DCF_XL, 2#10).
-define(FDS_XL, 2#0).
-define(HPIS_XL, 2#0).%disable high-pass


%% API
-export([verify/1, setup/1, verifyConfig/1, readAcc/1, readGy/1, readAccScaled/1]).

verify(Slot) ->
  <<"h">> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, 2#0001111:7>>, 1, 1).


setup(Slot) ->
  setupAcc(Slot),
  setupGy(Slot).
setupAcc(Slot) ->
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?CTRL_REG5_XL:7, ?DEC_XL:2, ?ZEN_XL:1, ?YEN_XL:1, ?XEN_XL:1, 0:3>>, 0, 0),
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?CTRL_REG6_XL:7, ?ODR_XL:3, ?FS_XL:2, ?BW_SCAL_ODR:1, ?BW_XL:2>>, 0, 0),
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?CTRL_REG7_XL:7, ?HR_XL:1, ?DCF_XL:2, 0:2, ?FDS_XL:1, 0:1, ?HPIS_XL:1>>, 0, 0).
setupGy(Slot) ->
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?CTRL_REG1_G:7, ?ODR_G:3, ?FS_G:2, 0:1, ?BW_G:2>>, 0, 0),
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?CTRL_REG2_G:7, 0:4, ?INT_SEL:2, ?OUT_SEL:2>>, 0, 0),
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?CTRL_REG3_G:7, ?LP_MODE:1, ?HP_EN:1, 0:2, ?HPCF_G:4>>, 0, 0),
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?ORIENT_CFG_G:7, 0:2, ?SIGNX_G:1, ?SIGNY_G:1, ?SIGNZ_G:1, ?ORIENT_G:3>>, 0, 0),
  grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#0:1, ?CTRL_REG4:7, 0:2, ?ZEN_G:1, ?YEN_G:1, ?XEN_G:1, 0:1, ?LIR_XL1:1, ?D4_XL1:1>>, 0, 0).

verifyConfig(Slot) ->
  <<?DEC_XL:2, ?ZEN_XL:1, ?YEN_XL:1, ?XEN_XL:1, 0:3>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, 2#0011111:7, 16#0>>, 1, 0),
  <<?ODR_XL:3, ?FS_XL:2, ?BW_SCAL_ODR:1, ?BW_XL:2>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, 2#0100000:7, 16#0>>, 1, 0),
  <<?HR_XL:1, ?DCF_XL:2, 0:2, ?FDS_XL:1, 0:1, ?HPIS_XL:1>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, 2#0100001:7, 16#0>>, 1, 0).

readAcc(Slot) ->
  <<AX:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?ACCX_REGL:7>>, 1, 1),
  <<BX:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?ACCX_REGH:7>>, 1, 1),
  <<X:16/little-signed-integer>> = <<AX, BX>>,
  <<AY:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?ACCY_REGL:7>>, 1, 1),
  <<BY:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?ACCY_REGH:7>>, 1, 1),
  <<Y:16/little-signed-integer>> = <<AY, BY>>,
  <<AZ:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?ACCZ_REGL:7>>, 1, 1),
  <<BZ:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?ACCZ_REGH:7>>, 1, 1),
  <<Z:16/little-signed-integer>> = <<AZ, BZ>>,
  {X, Y, Z}.
readAccScaled(Slot) ->
  {X, Y, Z} = readAcc(Slot),
  {X/16#4000, Y/16#4000, Z/16#4000}. %scale to 1g=1.0f
readGy(Slot) ->
  <<AX:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?GYX_REGL:7>>, 1, 1),
  <<BX:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?GYX_REGH:7>>, 1, 1),
  <<X:16/little-signed-integer>> = <<AX, BX>>,
  <<AY:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?GYY_REGL:7>>, 1, 1),
  <<BY:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?GYY_REGH:7>>, 1, 1),
  <<Y:16/little-signed-integer>> = <<AY, BY>>,
  <<AZ:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?GYZ_REGL:7>>, 1, 1),
  <<BZ:8>> = grisp_spi:send_recv(Slot, #{cpol => low, cpha => leading}, <<2#1:1, ?GYZ_REGH:7>>, 1, 1),
  <<Z:16/little-signed-integer>> = <<AZ, BZ>>,
  {X, Y, Z}.