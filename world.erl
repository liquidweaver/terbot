-module( world ).
-include( "world.hrl" ).
-include( "terpacket.hrl" ).
-compile( export_all ).

start(WorldGlobals = #tp_world_globals{} ) ->
   WorldState = #worldstate{ is_tileframe_important=dict_tile_frame_important(),
      world_data_table=ets:new(unused, [set, private, {read_concurrency, false}]),
      world_globals=WorldGlobals},
   register( world, spawn( ?MODULE, world_loop, [WorldState] ) ).

world_loop( WorldState ) ->
   receive
      {terpacket, 10, _} = SectionData ->
         {{TotalLength, X, Y}, TileSections} = decode_section( WorldState, SectionData ),
         ok = update_world_section(WorldState#worldstate.world_data_table, X, Y, TotalLength, TileSections);
      Unknown ->
         io:format( "world_loop: unknown message: ~p~n", [Unknown] )
   end,
   world_loop( WorldState ).


update_world_section( WorldDataTable, X, Y, Left, [TileSection|T] ) ->
   Copies = TileSection#tile_section.copies,
   [ ets:insert( WorldDataTable, { { X+Delta, Y }, TileSection#tile_section.tile} ) || Delta <- lists:seq( 0, Copies ) ],
   update_world_section( WorldDataTable, X + Copies + 1, Y, Left - (Copies + 1), T );

update_world_section( _WorldDataTable, _X, _Y, Left, [] ) when Left =:= 0 ->
   ok.

decode_section( WorldState=#worldstate{}, <<TotalLength:16/little-signed, X:32/little-signed, Y:32/little-signed, TileData/binary>> ) ->
   decode_section( WorldState, {TotalLength, X, Y}, [], TileData ).

decode_section( WorldState=#worldstate{}, MetaData, TileSections, <<Active:1, Lighted:1,Wall:1, Liquid:1, Wire:1, _Unused:3, TileBinary/binary>> ) ->
   InitialTile = #tile{ flag_active=Active, flag_lighted=Lighted, flag_wall=Wall, flag_liquid=Liquid, flag_wire=Wire},
   InitialTileSection = #tile_section{ tile=InitialTile }, 
   {TileSection, Rest} = lists:foldl( fun( DecodeFlags, {TileSection, Rest}  ) ->
            DecodeFlags( WorldState, TileSection, Rest )
      end, { InitialTileSection, TileBinary } ,
      [ fun parse_flag_active/3, fun parse_flag_wall/3, fun parse_flag_liquid/3, fun parse_tile_length/3] ),
   decode_section( MetaData, [TileSection|TileSections], Rest ).

decode_section( MetaData, Tiles, <<>> ) ->
   {MetaData, lists:reverse(Tiles)}.

parse_flag_active( WorldState, TileSection, Rest ) ->
   case TileSection#tile_section.tile of
      #tile{ flag_active=1 } ->
         <<Type:8, Rest1/binary>> = Rest,
         case is_tileframe_important( WorldState, Type ) of
            false ->
               { TileSection#tile_section.tile#tile{type=Type}, Rest1 };
            true ->
               <<FrameX:16/little-signed, FrameY:16/little-signed, Rest2/binary>> = Rest1,
               { TileSection#tile_section.tile#tile{ type=Type, framex=FrameX, framey=FrameY }, Rest2 }
         end;
      true ->
         {TileSection, Rest}
   end.

parse_flag_wall( _WorldState, TileSection, Rest ) ->
   case TileSection#tile_section.tile of
      #tile{ flag_wall=1 } ->
         <<Wall:8, Rest1/binary>> = Rest,
         { TileSection#tile_section.tile#tile{ wall=Wall }, Rest1 };
      true ->
         {TileSection, Rest}
   end.

parse_flag_liquid( _WorldState, TileSection, Rest ) ->
   case TileSection#tile_section.tile of
      #tile{ flag_liquid=1 } ->
         <<Liquid:8, IsLava:8, Rest1/binary>> = Rest,
         { TileSection#tile_section.tile#tile{ liquid=Liquid, lava=IsLava }, Rest1 };
      true ->
         {TileSection, Rest}
   end.

parse_tile_length( _WorldState, TileSection, Rest ) ->
   <<Copies:16/little-signed, Rest1>> = Rest,
   { TileSection#tile_section{ copies = Copies }, Rest1 }.

is_tileframe_important( TileId, DictTileImportant ) ->
   case dict:find( TileId, DictTileImportant ) of
      { ok, _Value } ->
         true;
      error ->
         false
   end.

dict_tile_frame_important() ->
   dict:from_list(
      [ { 3, true}, { 4, true}, { 5, true}, { 10, true},
         { 11, true}, { 12, true}, { 13, true}, { 14, true},
         { 15, true}, { 16, true}, { 17, true}, { 18, true},
         { 20, true}, { 21, true}, { 24, true}, { 26, true},
         { 27, true}, { 28, true}, { 29, true}, { 31, true},
         { 33, true}, { 34, true}, { 35, true}, { 36, true},
         { 42, true}, { 50, true}, { 55, true}, { 61, true},
         { 71, true}, { 72, true}, { 73, true}, { 74, true},
         { 77, true}, { 78, true}, { 79, true}, { 81, true},
         { 82, true}, { 83, true}, { 84, true}, { 85, true},
         { 86, true}, { 87, true}, { 88, true}, { 89, true},
         { 90, true}, { 91, true}, { 92, true}, { 93, true},
         { 94, true}, { 95, true}, { 96, true}, { 97, true},
         { 98, true}, { 99, true}, { 101, true}, { 102, true},
         { 103, true}, { 104, true}, { 105, true}, { 100, true},
         { 106, true}, { 110, true}, { 113, true}, { 114, true},
         { 125, true}, { 126, true}, { 128, true}, { 129, true},
         { 132, true}, { 133, true}, { 134, true}, { 135, true},
         { 141, true} ] ).
