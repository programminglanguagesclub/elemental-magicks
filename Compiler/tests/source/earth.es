/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  1    9         8
    2  5    9         4
    3  2    9         7
    4  2    9         7
    5  4    9         5
    6  1    9         8
    7  0    9         9
    8  0    9         9
    9  0    9         9
spells:
       have objective missing
lvl 1  0    3         3
    2  0    3         3
    3  0    3         3
    4  0    3         3
    5  0    3         3
    6  0    3         3
    7  0    3         3
    8  0    3         3
    9  0    3         3


*/

unit "Treant Watchman" earth level : 1 hp : 40 attack : 10 defense : 10 speed : 1 range : 1 soulPoints : 2
counter : (engagement self) += 2; (permanent defense self) += 5;
soulSkill : (friendly thoughts) += cardinality (x in friendly field where not dead x);, for each x in friendly field, (engagement x) += 1;


unit "Forest Druid" earth level : 2 hp : 30 attack : 20 defense : 0 speed : 1 range : 2 soulPoints : 1
action : cost : 1 select x in friendly field then {(max hp x) += earth friendly * 10; (hp x) := max hp x;}
soulSkill : select x in friendly field then {(max hp x) += earth friendly * 10; (hp x) := max hp x;}

unit "Forest Pixie" earth level : 2 hp : 20 attack : 10 defense : 0 speed : 5 range : 3 soulPoints : 2
action : cost : 1 for each x in friendly field, (temporary attack x) := hp x;
soulSkill : cost : 1 for each x in friendly field, (temporary attack x) := hp x;

unit "Fox Musician" earth level : 2 hp : 30 attack : 10 defense : 0 speed : 2 range : 3 soulPoints : 2
action : for each x in friendly field, (permanent attack x) += 5;
soulSkill : for each x in friendly field, (permanent attack x) += 50;, for each x in enemy field, (permanent attack x) += 50;


unit "Fox Wizard" earth level : 2 hp : 40 attack : 0 defense : 0 speed : 2 range : 2 soulPoints : 2
start : (water friendly) -= 1;
action : cost : 1 (water friendly) += 3;
soulSkill : select x in friendly field then {(permanent defense x) += water friendly * 5;}
/*make sure that I am not selecting dead units when not applicable*/
/*note modifying permanent stats causes the same command to be performed on the temporary stats.*/

unit "Princess of Snakes" earth level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 1
action : select x in enemy field then {(hp x) := 0;}
soulSkill : cost : 7 for each x in enemy field, (hp x) := 0;

unit "Dryad Elementalist" earth level : 3 hp : 60 attack : 20 defense : 0 speed : 2 range : 2 soulPoints : 1
action : select x in friendly field then {(max hp x) += max hp x - hp x;}
action : select x in friendly field then {damage unit on the same square as x max hp x - hp x;}
soulSkill : (earth friendly) += 1; select x in friendly field then {(max hp x) += 20; (hp x) += 30;}

unit "Ent Sapling" earth level : 3 hp : 35 attack : 25 defense : 10 speed : 2 range : 1 soulPoints : 1 
auto : (max hp self) += 5; (hp self) += 15;
soulSkill : select x in friendly field then {(max hp x) += 50; (hp x) += 50;}

unit "Dragontaur" earth level : 4 hp : 60 attack : 40 defense : 10 speed : 2 range : 2 soulPoints : 2
action : for each x in enemy field where x in range self, damage x temporary attack self / 2;
soulSkill : select x in friendly field then {(permanent defense x) += earth friendly * 5;}

unit "Ent Dragon" earth level : 4 hp : 70 attack : 20 defense : 10 speed : 2 range : 1 soulPoints : 2
counter : for each x in friendly field, (permanent attack x) += 10;
soulSkill : for each x in friendly field, (permanent attack x) += permanent defense x;

unit "Chimera" earth level : 5 hp : 60 attack : 0 defense : 0 speed : 5 range : 1 soulPoints : 2
start : for each x in friendly field, (temporary attack self) += temporary attack x;
soulSkill : select x in friendly field, y in enemy field then {damage y temporary attack x;}

unit "Earth Warbeast" earth level : 5 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 2
end : (hp self) := max hp self;
auto : (permanent attack self) := 15;
soulSkill : for each x in friendly field, (hp x) := max hp x;

unit "Ent Warbeast" earth level : 5 hp : 60 attack : 5 defense : 10 speed : 4 range : 1 soulPoints : 1
auto : (permanent attack self) += temporary attack self;
soulSkill : for each x in friendly field, (permanent attack x) += temporary attack x;


unit "Gold Spawn" earth level : 5 hp : 100 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
counter : (friendly thoughts) += 1;
soulSkill : (friendly thoughts) += cardinality (x in enemy field where engagement x = 0); /*and not dead...*/

unit "Forest Dragon" earth level : 6 hp : 100 attack : 20 defense : 0 speed : 2 range : 5 soulPoints : 2
start : for each x in friendly field, (temporary defense x) += 10;
auto : for each x in friendly field, (max hp x) += 15; (hp x) += 15;
action : select x in friendly field then {(hp x) := max hp x;}
/*have action that fully heals a target unit... might want to make this not able to target self...?*/
soulSkill : for each x in friendly field, (max hp x) += 15; (hp x) += 15;



unit "God of Earth" earth level : 9 hp : 150 attack : 70 defense : 10 speed : 2 range : 5 soulPoints : 2
start : for each x in friendly field, (max hp x) += 10; (hp x) += 20; (permanent attack x) += 10; (temporary defense x) += 10;
end : for each x in friendly field, (hp x) := max hp x;
spawn : for each x in friendly field, (hp x) := max hp x;
soulSkill : for each x in friendly field, (hp x) := max hp x;






