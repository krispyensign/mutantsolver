pragma Ada_2022;
with Ada.Calendar;

package Core is
   package calendar renames Ada.Calendar;

   type Candle is record
      Complete                                                           :
        Boolean;
      Time                                                               :
        calendar.Time;
      Volume                                                             :
        Integer;
      WMA                                                                :
        Long_Float;
      ATR                                                                :
        Long_Float;
      Bid_Open, Bid_High, Bid_Low, Bid_Close                             :
        Long_Float;
      Mid_Open, Mid_High, Mid_Low, Mid_Close                             :
        Long_Float;
      Ask_Open, Ask_High, Ask_Low, Ask_Close                             :
        Long_Float;
      HA_Bid_Open, HA_Bid_High, HA_Bid_Low, HA_Bid_Close                 :
        Long_Float;
      HA_Mid_Open, HA_Mid_High, HA_Mid_Low, HA_Mid_Close                 :
        Long_Float;
      HA_Ask_Open, HA_Ask_High, HA_Ask_Low, HA_Ask_Close                 :
        Long_Float;
      WMA_Bid_Open, WMA_Bid_High, WMA_Bid_Low, WMA_Bid_Close             :
        Long_Float;
      WMA_Mid_Open, WMA_Mid_High, WMA_Mid_Low, WMA_Mid_Close             :
        Long_Float;
      WMA_Ask_Open, WMA_Ask_High, WMA_Ask_Low, WMA_Ask_Close             :
        Long_Float;
      WMA_HA_Bid_Open, WMA_HA_Bid_High, WMA_HA_Bid_Low, WMA_HA_Bid_Close :
        Long_Float;
      WMA_HA_Mid_Open, WMA_HA_Mid_High, WMA_HA_Mid_Low, WMA_HA_Mid_Close :
        Long_Float;
      WMA_HA_Ask_Open, WMA_HA_Ask_High, WMA_HA_Ask_Low, WMA_HA_Ask_Close :
        Long_Float;
   end record;

   type Candles is array (Positive range <>) of Candle;

   function Make_HA_Candle
     (current_candle : Candle; previous_candle : Candle) return Candle;

end Core;
