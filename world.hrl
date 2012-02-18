-record( tile, {
      flag_active=0,
      flag_lighted=0,
      flag_wall=0,
      flag_liquid=0,
      flag_wire=0,
      type=0,
      framex,
      framey,
      wall=0,
      liquid=0,
      water,
      lava
   }).

-record( tile_section, {
      tile,
      copies
   }).

-record( worldstate, {
      is_tileframe_important,
      world_data_table,
      world_globals
   }).
