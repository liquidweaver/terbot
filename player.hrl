-record( player, 
   { name="Liz",
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
      difficulty=0,
      inventory= [], % 60 items
      buffs= [], %10 types
      current_life=60,
      max_life=60,
      current_mana=0,
      max_mana=0,
      positionx=0,
      positiony=0
   }).

-record( item, { id=0, stack=0, prefix=0 } ).
