pragma Ada_2022;

package Common is

   type Real_Array is array (Positive range <>) of Long_Float;
   type Real_Array_Ptr is not null access all Real_Array;
   type Pool_Key is
     (Time,

      Volume,
      ATR,

      Mid_Open,
      Mid_High,
      Mid_Low,
      Mid_Close,
      Ask_Open,
      Ask_High,
      Ask_Low,
      Ask_Close,
      Bid_Open,
      Bid_High,
      Bid_Low,
      Bid_Close,
      HA_Mid_Open,
      HA_Mid_High,
      HA_Mid_Low,
      HA_Mid_Close,
      HA_Ask_Open,
      HA_Ask_High,
      HA_Ask_Low,
      HA_Ask_Close,
      HA_Bid_Open,
      HA_Bid_High,
      HA_Bid_Low,
      HA_Bid_Close,

      WMA_Mid_Open,
      WMA_Mid_High,
      WMA_Mid_Low,
      WMA_Mid_Close,
      WMA_Ask_Open,
      WMA_Ask_High,
      WMA_Ask_Low,
      WMA_Ask_Close,
      WMA_Bid_Open,
      WMA_Bid_High,
      WMA_Bid_Low,
      WMA_Bid_Close,
      WMA_HA_Mid_Open,
      WMA_HA_Mid_High,
      WMA_HA_Mid_Low,
      WMA_HA_Mid_Close,
      WMA_HA_Ask_Open,
      WMA_HA_Ask_High,
      WMA_HA_Ask_Low,
      WMA_HA_Ask_Close,
      WMA_HA_Bid_Open,
      WMA_HA_Bid_High,
      WMA_HA_Bid_Low,
      WMA_HA_Bid_Close);

   subtype WMA_Source_Key is Pool_Key range WMA_Mid_Open .. WMA_HA_Bid_Close;
   subtype Candle_Key is Pool_Key range Mid_Open .. HA_Bid_Close;
   subtype Time_Key is Pool_Key range Time .. Time;
   subtype Other_Key is Pool_Key range Volume .. ATR;

   type Keyed_Lane is
     array (Common.Pool_Key range Common.Pool_Key'Range) of Long_Float;
   type Row_Pool is array (Positive range <>) of Keyed_Lane;

   Take_Profit_Multipliers : constant array (Positive range <>) of Float :=
     [0.0,
      0.1,
      0.2,
      0.3,
      0.4,
      0.5,
      0.6,
      0.7,
      0.8,
      0.9,
      1.0,
      2.0,
      3.0,
      4.0,
      5.0,
      6.0,
      7.0,
      8.0];

   Stop_Loss_Multipliers : constant array (Positive range <>) of Float :=
     [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 2.0, 3.0, 4.0];
end Common;
