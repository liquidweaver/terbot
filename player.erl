-module( player ).
-include( "player.hrl" ).
-compile( export_all ).

start( PlayerID ) ->
   %%TODO: Load in player
   Player = #player{ id=PlayerID, inventory= lists:duplicate(60, #item{}), buffs= lists:duplicate(10, 0) },
   register( ?MODULE, spawn_link( ?MODULE, player_loop, [Player] ) ).

player_loop( Player = #player{} ) ->
   receive
      {From, get_player} ->
         From ! Player
   end,
   player_loop( Player ).


get_player() ->
   player ! { self(), get_player },
   receive
      Player = #player{} ->
         Player
   end.
