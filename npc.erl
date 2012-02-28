-module( npc ).
-include( "terpacket.hrl" ).
-import( terpacket ).
-compile( export_all ).

start() ->
   Npcs = dict:new(),
   register( ?MODULE, spawn( ?MODULE, npc_loop, [Npcs] ) ).

npc_loop( Npcs ) ->
   receive
      {terpacket, 23, _} = NpcUpdatePacket ->
         PlayerUpdate = terpacket:terpacket_to_record( NpcUpdatePacket ),
         Npcs1 =  dict:store( PlayerUpdate#tp_npc_update.npcid, PlayerUpdate, Npcs ),
         npc_loop( Npcs1 );
      {terpacket, 56, _} = NpcNamePacket ->
         NameUpdate = terpacket:terpacket_to_record( NpcNamePacket ),
         io:format( "npc: tossed npc name update (ID:~p,Name:~p)~n", [ NameUpdate#tp_update_npc_name.npcid, NameUpdate#tp_update_npc_name.name ] ),
         npc_loop( Npcs );
      {terpacket, 60, _} = NpcHomeInfoPacket ->
         HomeInfoUpdate = terpacket:terpacket_to_record( NpcHomeInfoPacket ),
         io:format( "npc: tossed home into update (ID:~p,TileX:~p,TileY:~p,Homeless:~p)~n",
               [ HomeInfoUpdate#tp_npc_home_info_update.npcid,
                 HomeInfoUpdate#tp_npc_home_info_update.hometilex,
                 HomeInfoUpdate#tp_npc_home_info_update.hometiley,
                 HomeInfoUpdate#tp_npc_home_info_update.homeless ] ),
         npc_loop( Npcs );
      Unknown ->
         io:format( "npc: Unknown packet: ~p~n", [Unknown] ),
         npc_loop( Npcs )
   end.

