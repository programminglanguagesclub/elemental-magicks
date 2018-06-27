unit "Ent Sapling" earth level : 3 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 1
 soulSkill : select x in friendly field
  then {
   hp self := 0;
   select y in enemy field
    then {
     hp y := hp x
    }
  }
  otherwise {
   hp self := 5
  }


