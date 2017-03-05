unit "Ent Sapling" earth level : 3 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 1
 soulSkill :
  select x in friendly field
   then {
    (hp self) := 0;
    select y in enemy field then { (hp y) := hp x; }
   }
   if unable { (hp self) := 5; }

/*
 *
 * For output purposes, I need to keep track of whether I have curly brackets or not. 
 * In the case where I only have a single statement, I do not need the curly brackets, but I should keep track of whether it was bracketed for output purposes.
 *
 *
 * Should have a mechanism for excluding units from being selected. For instance, in the above skill I might want to say that x and self must be different.
 *
 *
 *
 *
 *
 * I also want a mechanism for recognizing impossible then / else branches.
 *
 *
 * */
