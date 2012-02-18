-module( terpacket ).
-include( "terpacket.hrl" ).
-export( [terpacket_to_record/1] ).

terpacket_to_record( {terpacket, 1, ConnectRequest} ) ->
   #tp_connect_request{ version=binary_to_list( ConnectRequest ) };

terpacket_to_record( {terpacket, 4, <<PlayerID:8, Hair:8, Male:8, HairColor:24, SkinColor:24,
      EyeColor:24, ShirtColor:24, UnderShirtColor:24, PantsColor:24,
      ShoeColor:24, Difficulty:8, Name/binary>>} ) ->
   #tp_player_info{ name=binary_to_list(Name), id=PlayerID, hair=Hair, sex=Male, hair_rgb=HairColor,
      skin_rgb=SkinColor, eye_rgb=EyeColor, shirt_rgb=ShirtColor, undershirt_rgb=UnderShirtColor,
      pants_rgb=PantsColor, shoe_rgb=ShoeColor, difficulty=Difficulty };

terpacket_to_record( {terpacket, 5, <<PlayerID:8, SlotID:8, Stack:8, Prefix:8, ItemID:16/little-signed>>} ) ->
   #tp_player_inventory_slot{ playerid=PlayerID, slotid=SlotID, stack=Stack, prefix=Prefix, itemid=ItemID };

terpacket_to_record( {terpacket, 7, <<Time:32/little-signed, DayTime:8, MoonPhase:8, BloodMoon:8,
      MaxTilesX:32/little-signed, MaxTilesY:32/little-signed, 
      SpawnX:32/little-signed, SpawnY:32/little-signed,
      WorldSurface:32/little-signed, RockLayer:32/little-signed,
      WorldID:32/little-signed, OrbSmashed:1, Boss1Dead:1, Boss2Dead:1,
      Boss3Dead:1, HardMode:1, ClownDead:1, _Unused:2, 
      WorldName/binary>> } ) ->
   #tp_world_globals{ time=Time, daytime=DayTime, moonphase=MoonPhase, bloodmoon=BloodMoon, maxtilesx=MaxTilesX,
      maxtilesy=MaxTilesY, spawnx=SpawnX, spawny=SpawnY, worldsurface=WorldSurface, 
      rocklayer=RockLayer, worldid=WorldID, orbsmashed=OrbSmashed, boss1dead=Boss1Dead,
      boss2dead=Boss2Dead, boss3dead=Boss3Dead, hardmode=HardMode, clowndead=ClownDead,
      worldname=WorldName };

terpacket_to_record( {terpacket, 13, <<PlayerID:8, ControlUp:1, ControlDown:1, ControlLeft:1, ControlRight:1,
      ControlJump:1,ControlUseItem:1,ControlDirection:1,_UnusedBit:1,
      SelectedItem:8, PositionX:32/little-float,PositionY:32/little-float,
      VelocityX:32/little-float, VelocityY:32/little-float>>} )->
   #tp_update_player{ playerid=PlayerID, controlup=ControlUp,controldown=ControlDown, controlleft=ControlLeft,
      controlright=ControlRight, controljump=ControlJump, controluseitem=ControlUseItem,
      controldirection=ControlDirection, selecteditem=SelectedItem, positionx=PositionX,
      positiony=PositionY, velocityx=VelocityX, velocityy=VelocityY};

terpacket_to_record( {terpacket, 14, <<PlayerID:8, Active:8>> } ) ->
   #tp_active_player{ playerid=PlayerID, active=Active };

terpacket_to_record( {terpacket, 16, <<PlayerID:8, Health:16/little-signed, MaxHealth:16/little-signed>>} ) ->
   #tp_player_hp{ playerid=PlayerID, life=Health, lifemax=MaxHealth };

%TODO: Finish this
terpacket_to_record( {terpacket, 20, <<Size:16/little-signed, TileX:32/little-signed, TileY:32/little-signed,
      TileData/binary>>} ) ->
   #tp_send_tile_square{size=Size, tilex=TileX, tiley=TileY, tiledata=TileData};

terpacket_to_record( {terpacket, 21, <<ItemID:16/little-signed, PositionX:32/float, PositionY:32/float,
      VelocityX:32/float, VelocityY:32/float, Stacks:8, Prefix:8,
      Type:16/little-signed>> } ) ->
   #tp_update_item{ itemid=ItemID, positionx=PositionX, positiony=PositionY, velocityx=VelocityX,
      velocityy=VelocityY, stacks=Stacks, prefix=Prefix, type=Type };

terpacket_to_record( {terpacket, 23, <<NPCID:16/little-signed, PositionX:32/little-float,
      PositionY:32/little-float, VelocityX:32/little-float,
      VelocityY:32/little-float, Target:16/little-signed,
      Direction:8, DirectionY:8, Life:32/little-signed,
      AI:16/binary, Type:16/little-signed>> } ) ->
   #tp_npc_update {npcid=NPCID, positionx=PositionX, positiony=PositionY, velocityx=VelocityX,
      velocityy=VelocityY, target=Target, direction=Direction, directiony=DirectionY, 
      life=Life, ai=AI, type=Type};

terpacket_to_record( {terpacket, 36, <<PlayerID:8, ZoneEvil:8, ZoneMeteor:8, ZoneDungeon:8,
      ZoneJungle:8, ZoneHoly:8>> } ) ->
   #tp_currently_in_zone{ playerid=PlayerID, zoneevil=ZoneEvil, zonemeteor=ZoneMeteor, zonedungeon=ZoneDungeon,
      zonejungle=ZoneJungle, zoneholy=ZoneHoly };

terpacket_to_record( {terpacket, 56, <<NPCID:16/little-signed, Name/binary>> } ) ->
   #tp_update_npc_name{ npcid=NPCID, name=Name };

terpacket_to_record( {terpacket, 60, <<NPCID:16/little-signed, HomeTileX:16/little-signed,
      HomeTileY:16/little-signed, Homeless:8>>} ) ->
   #tp_npc_home_info_update{ npcid=NPCID, hometilex=HomeTileX, hometiley=HomeTileY, homeless=Homeless};

terpacket_to_record( {terpacket, N, Payload} ) ->
   { unknown, N, Payload, byte_size(Payload)}.
