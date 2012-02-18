-record( tp_connect_request, {
      version
   }).

-record( tp_disconnect_client, {
      reason
   }).

-record( tp_continue_connecting, {
      playerid
   }).

-record( tp_player_info, {
      name="Liz",
      id=1,
      hair=23,
      sex=0,
      hair_rgb= <<255,166,0>>,
      skin_rgb= <<189,183,107>>,
      eye_rgb= <<0,255,255>>,
      shirt_rgb= <<255,0,0>>,
      undershirt_rgb= <<255,0,0>>,
      pants_rgb= <<64,64,64>>,
      shoe_rgb= <<160,105,60>>,
      difficulty=0
   }).

-record( tp_player_inventory_slot, {
      playerid,
      slotid,
      stack,
      prefix,
      itemid
   }).

-record( tp_continue_connecting_2, {
      nothing=nothing
   }).

-record( tp_world_globals, {
      time,
      daytime,
      moonphase,
      bloodmoon,
      maxtilesx,
      maxtilesy,
      spawnx,
      spawny,
      worldsurface,
      rocklayer,
      worldid,
      orbsmashed,
      boss1dead,
      boss2dead,
      boss3dead,
      hardmode,
      clowndead,
      worldname
   }).

-record( tp_get_section, {
      x,
      y
   }).

-record( tp_status, {
      number,
      status
   }).

-record(  tp_send_section, {
      width,
      x,
      y,
      tiles
   }).

-record( tp_section_tile_frame, {
      startx,
      starty,
      endx,
      endy
   }).

-record( tp_spawn_player, {
      playerid,
      spawnx,
      spawny
   }).

-record( tp_update_player, {
      playerid,
      controlup,
      controldown,
      controlleft,
      controlright,
      controljump,
      controluseitem,
      controldirection,
      selecteditem,
      positionx,
      positiony,
      velocityx,
      velocityy
   }).

-record( tp_active_player, {
      playerid,
      active
   }).

-record( tp_request_sync_players, {
      nothing=nothing
   }).

-record( tp_player_hp, {
      playerid,
      life,
      lifemax
   }).

-record( tp_modify_tile, {
      action,
      x,
      y,
      tiletype,
      fail,
      style
   }).

-record( tp_time, {
      daytime,
      time,
      sunmody,
      moonmody
   }).

-record( tp_open_close_door, {
      closed,
      tilex,
      tiley
   }).

%20 (0x15)
-record( tp_send_tile_square, {
      size,
      tilex,
      tiley,
      tiledata
   }).

-record( tp_update_item, {
      itemid,
      positionx,
      positiony,
      velocityx,
      velocityy,
      stacks,
      prefix,
      type
   }).

-record( tp_item_owner, {
      itemid,
      owner 
   }).

-record( tp_npc_update, {
      npcid,
      positionx,
      positiony,
      velocityx,
      velocityy,
      target,
      direction,
      directiony,
      life,
      ai,
      type
   }).

-record( tp_npc_strike_1, {
      npcid,
      playerid
   }).

-record( tp_chat_message, {
      playerid,
      color,
      message
   }).

-record( tp_player_damage, {
      playerid,
      hitdirection,
      damage,
      pvp,
      crit,
      deathtext
   }).

-record( tp_projectile_update, {
      projectileid,
      positionx,
      positiony,
      velocityx,
      velocityy,
      knockback,
      damage,
      owner,
      type,
      ai
   }).

-record( tp_npc_strike_2, {
      npcid,
      damage,
      knockback,
      direction,
      crit
   }).

-record( tp_projectile_destroy, {
      projectile_id,
      owner
   }).

-record( tp_toggle_pvp, {
      playerid,
      pvp_on_off
   }).

-record( tp_get_chest_contents, {
      tilex,
      tiley
   }).

-record( tp_new_chest_item, {
      chestid,
      itemslot,
      stack,
      prefix,
      type
   }).

-record( tp_set_active_chest, {
      chestid,
      chestx,
      chesty
   }).

-record( tp_kill_chest, {
      tilex,
      tiley
   }).

-record( tp_heal_effect, {
      playerid,
      heal_amount
   }).

-record( tp_currently_in_zone, {
      playerid,
      zoneevil,
      zonemeteor,
      zonedungeon,
      zonejungle,
      zoneholy
   }).

-record( tp_request_password, {
      nothing=nothing
   }).

-record( tp_send_password, {
      password
   }).

-record( tp_remove_item_owner, {
      itemindex
   }).

-record( tp_set_active_npc, {
      npcindex,
      npctalk
   }).

-record( tp_player_animation, {
      playerid,
      itemrotation,
      itemanimation
   }).

-record( tp_player_mana, {
      playerid,
      mana,
      manamax
   }).

-record( tp_mana_effect, {
      playerid,
      manaamount
   }).

-record( tp_kill_me, {
      playerid,
      hitdirection,
      damage,
      pvp,
      deathtext
   }).

-record( tp_player_change_team, {
      playerid,
      team
   }).

-record( tp_read_sign, {
      x,
      y
   }).

-record( tp_update_sign, {
      signid,
      x,
      y,
      text
   }).

-record( tp_set_liquid, {
      tilex,
      tiley,
      liquid,
      lava
   }).

-record( tp_first_player_spawn, {
      nothing=nothing
   }).

-record( tp_player_set_buff, {
      playerid,
      buff0,
      buff1,
      buff2,
      buff3,
      buff4,
      buff5,
      buff6,
      buff7,
      buff8,
      buff9
   }).

-record( tp_special_npc_effect, {
      playerid,
      type
   }).

-record( tp_chest_unlock, {
      unused,
      unlock,
      x,
      y
   }).

-record( tp_add_npc_buff, {
      npcindex,
      buff,
      time
   }).

-record( tp_update_npc_buff, {
      npcindex,
      buff,
      time
   }).

-record( tp_add_player_buff, {
      playerid,
      buff,
      time
   }).

-record( tp_update_npc_name, {
      npcid,
      name
   }).

-record( tp_update_good_evil_percentage, {
      good,
      evil
   }).

-record( tp_play_harp, {
      playerid, 
      note
   }).

-record( tp_hit_switch, {
      x,
      y
   }).

-record( tp_npc_home_info_update, {
      npcid,
      hometilex,
      hometiley,
      homeless
   }).
