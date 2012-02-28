-module( player ).
-include( "player.hrl" ).
-compile( export_all ).

start( PlayerID ) ->
   %%TODO: Load in player
   Player = #player{ id=PlayerID, inventory= lists:duplicate(60, #item{}), buffs= lists:duplicate(10, 0) },
   register( ?MODULE, spawn_link( ?MODULE, player_loop, [Player] ) ).


get_player() ->
   player ! { self(), get_player },
   receive
      Player = #player{} ->
         Player
   end.

notify_spawn( X, Y ) ->
   player ! {spawn, X, Y},
   ok.

player_loop( Player = #player{} ) ->
   receive
      {From, get_player} ->
         From ! Player,
         player_loop( Player );
      {spawn, _X, _Y} = Spawn ->
         Player1 = handle_spawn( Player, Spawn ),
         player_loop( Player1 )
   end.

handle_spawn( Player, {spawn, X, Y} ) 
      when Player#player.positionx == 0, Player#player.positiony == 0 ->
   Player#player{ positionx=X, positiony=Y };
handle_spawn( Player, {spawn, _X, _Y} ) ->
   Player.
