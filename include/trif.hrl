

-type e() :: {true, pos_integer()}
           | {false, pos_integer()}
           | {symbol, pos_integer(), binary()}
           | {null, pos_integer()}
           | {cons, pos_integer(), e(), e()}.