-module(terbot).
-compile(export_all).
-import( gen_tcp ).
-import( terpacket ).
-import( terproto ).
-include( "player.hrl" ).
-define( VERSION, "39" ).

start() ->
   terproto:start(),
   login().

login() ->
   terproto:send( packet_signon() ),
   {terpacket, 3, <<PlayerID:32/little-signed>> } = terproto:recv( 3 ),
   player:start( PlayerID ),
   Player = player:get_player(),
   terproto:send( packet_player_info( Player ) ),
   terproto:send( packet_player_life( Player ) ),
   terproto:send( packet_player_mana( Player ) ),
   terproto:send( packet_player_buff( Player ) ),
   lists:foreach( fun( { Item, N } ) -> terproto:send( packet_inventory_item( Player, N, Item ) ) end,
      lists:zip( Player#player.inventory, lists:seq( 0, 59 ) ) ),
   terproto:send( packet_contine_connecting() ),
   {terpacket, 7, WorldDataBin} = terproto:recv( 7 ),
   WorldData = terpacket:terpacket_to_record( {terpacket, 7, WorldDataBin} ), 
   world:start( WorldData ),
   terproto:send( packet_get_section( -1, -1 ) ),
   terproto:recv( 49 ), %%OK to spawn in
   router_loop().

router_loop() ->
   receive
      {terpacket, 10, _} = TileSectionUpdate ->
         world ! TileSectionUpdate;
      {terpacket, 23, _} = NpcUpdatePacket ->
         npc ! NpcUpdatePacket;
      {terpacket, 48, _} = LiquidUpdate ->
         world ! LiquidUpdate;
      {terpacket, 60, _} = NpcNameUpdate ->
         npc ! NpcNameUpdate;
      {terpacket, N, Data} ->
         io:format( "Received Uknown terpacket [Type:~p, Data:~p]~n", [N, Data] )
   end,
   router_loop().

packet_get_section( X, Y ) ->
   {terpacket, 8, [ <<X:32/little-signed, Y:32/little-signed>>] }.

packet_signon() ->
   {terpacket, 1, "Terraria" ++ ?VERSION }.

packet_contine_connecting() ->
   {terpacket, 6, [] }.

packet_player_info( #player{} = Player ) ->
   {terpacket, 4, [ <<(Player#player.id):8>>, <<(Player#player.hair):8>>,
         <<(Player#player.sex):8>>, Player#player.hair_rgb,
         Player#player.skin_rgb, Player#player.eye_rgb,
         Player#player.shirt_rgb, Player#player.undershirt_rgb,
         Player#player.pants_rgb, Player#player.shoe_rgb,
         <<(Player#player.difficulty):8>>, Player#player.name] }.

packet_player_life( #player{ id=ID, current_life=Life, max_life=Maxlife } ) ->
   {terpacket, 16, [ <<ID:8>>, <<Life:16/little-signed>>, <<Maxlife:16/little-signed>> ] }.

packet_player_mana(  #player{ id=ID, current_mana=Mana, max_mana=Maxmana }  ) ->
   {terpacket, 16, [ <<ID:8>>, <<Mana:16/little-signed>>, <<Maxmana:16/little-signed>> ] }.

packet_player_buff( #player{ buffs=Buffs } ) ->
   {terpacket, 50, [ <<X:8>> || X <- Buffs ] }.

packet_inventory_item( #player{ id=PlayerID }, SlotNum, #item{ stack=Stack, prefix=Prefix, id=ItemID } ) ->
   {terpacket, 5, [ <<PlayerID:8>>, <<SlotNum:8>>, <<Stack:8>>, <<Prefix:8>>, <<ItemID:16/little-signed>> ] }.
