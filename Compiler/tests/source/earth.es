
unit "Guardian Toad" earth level : 1 hp : 30 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
counter : for each x in friendly field, (max hp x) += 5; (hp x) += 5;
spawn : cost : 1 (earth friendly) += 1; (water friendly) += 1;
soulSkill : cost : 5 select x in friendly field then {(max hp x) += 200; (hp x) := max hp x;}

unit "Fox Wizard" earth level : 2 hp : 40 attack : 0 defense : 0 speed : 2 range : 2 soulPoints : 2
start : (water friendly) -= 1;
action : cost : 1 (water friendly) += 3;
soulSkill : select x in friendly field then {(permanent defense x) += water friendly * 5;}
/*make sure that I am not selecting dead units when not applicable*/
/*note modifying permanent stats causes the same command to be performed on the temporary stats.*/

unit "Princess of Snakes" earth level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 1
action : select x in enemy field then {(hp x) := 0;}
soulSkill : cost : 7 for each x in enemy field, (hp x) := 0;

unit "Ent Sapling" earth level : 3 hp : 35 attack : 25 defense : 10 speed : 2 range : 1 soulPoints : 1 
auto : (max hp self) += 5; (hp self) += 15;
soulSkill : select x in friendly field then {(max hp x) += 50; (hp x) += 50;}

unit "Chimera" earth level : 5 hp : 60 attack : 0 defense : 0 speed : 5 range : 1 soulPoints : 2
start : for each x in friendly field, (temporary attack self) += temporary attack x;
soulSkill : select x in friendly field, y in enemy field then {damage y temporary attack x;}

unit "Earth Warbeast" earth level : 5 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 2
end : (hp self) := max hp self;
auto : (permanent attack self) := 15;
soulSkill : for each x in friendly field, (hp x) := max hp x;


unit "Gold Spawn" earth level : 5 hp : 100 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
counter : (friendly thoughts) += 1;
soulSkill : (friendly thoughts) += cardinality (x in enemy field where engagement x = 0); /*and not dead...*/

unit "Forest Dragon" earth level : 6 hp : 100 attack : 20 defense : 0 speed : 2 range : 5 soulPoints : 2
start : for each x in friendly field, (temporary defense x) += 10;
auto : for each x in friendly field, (max hp x) += 15; (hp x) += 15;
action : select x in friendly field then {(hp x) := max hp x;}
/*have action that fully heals a target unit... might want to make this not able to target self...?*/
soulSkill : for each x in friendly field, (max hp x) += 15; (hp x) += 15;














