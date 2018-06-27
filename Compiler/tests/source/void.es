/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  1    9         8
    2  3    9         6
    3  4    9         5
    4  1    9         8
    5  3    9         6
    6  0    9         9
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



unit "Voidling" void level : 1 hp : 30 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
start : for each x in enemy field, (temporary defense x) -= 5;
soulSkill : for each x in enemy field, (permanent defense x) := 0;, (friendly thoughts) += 1;

unit "Combat Sorceress of the Void" void level : 2 hp : 30 attack : 20 defense : 0 speed : 2 range : 2 soulPoints : 2
action : cost : 2 for each x in enemy field, (hp x) -= temporary attack x;
soulSkill : cost : 2 for each x in enemy field, (hp x) -= temporary attack x;


unit "Corrupted Watchtower" void level : 2 hp : 30 attack : 0 defense : 0 speed : 1 range : 4 soulPoints : 1
start : for each x in enemy field, (permanent speed x) -= 1;
auto : (max hp self) -= 10;
soulSkill : for each x in enemy field, (permanent range x) := 1;



unit "Princess of the Void" void level : 2 hp : 40 attack : 0 defense : 0 speed : 2 range : 3 soulPoints : 2
start : for each x in enemy field, (temporary defense x) := 0;
soulSkill : for each x in enemy field, (permanent defense x) := 0;

unit "Clown of Horror" void level : 3 hp : 40 attack : 10 defense : 0 speed : 1 range : 2 soulPoints : 2
auto : for each x in enemy field, (permanent attack x) -= 5;
soulSkill : for each x in enemy field, damage x temporary attack x;


unit "Demon Whisperer" void level : 3 hp : 60 attack : 20 defense : 0 speed : 2 range : 2 soulPoints : 1
auto : (friendly thoughts) -= 1;
action : (earth enemy) -= 1; (fire enemy) -= 1; (water enemy) -= 1; (air enemy) -= 1; (spirit enemy) -= 1; (void enemy) -= 1;
soulSkill : (enemy thoughts) -= 2;
/*I need an "all schools" option*/


unit "Mutant Pig" void level : 3 hp : 60 attack : 10 defense : 0 speed : 3 range : 1 soulPoints : 1
death : (max hp self) -= 10; (hp self) := max hp self;
soulSkill : cost : 1 select x in friendly field then {(permanent attack x) *= 2; (hp x) := max hp x;}

unit "Serpent Swordswoman of the Void" void level : 3 hp : 50 attack : 40 defense : 0 speed : 2 range : 1 soulPoints : 1
action : for each x in enemy field where x in range self, (hp x) := 0;
soulSkill : cost : 6 select x in enemy field dead x then {send x to graveyard;}

unit "Vicious Soul Reaper" void level : 4 hp : 45 attack : 40 defense : 10 speed : 4 range : 1 soulPoints : 2
action : select x in enemy field then{damage x temporary attack self / 2;}
action : for each x in enemy field, (hp x) := 0;
/*DUMMIES*/
soulSkill : select x in enemy field dead x then{send x to graveyard;}
/*Do damage [attack / 2] to target enemy unit in range. If it has 0 or fewer hp, send it to the first position in its graveyard.
Action SkillDevastating Strike2 Thoughts
Do damage [current attack * 2] to target enemy unit in range.
*/

unit "Death Colossus" void level : 5 hp : 90 attack : 30 defense : 0 speed : 2 range : 3 soulPoints : 2
counter : for each x in enemy field where not dead x, damage x cardinality (y in enemy field where not dead y);
auto : (hp self) += cardinality (x in enemy field where dead x) * 10; (permanent attack self) += cardinality (x in friendly field where dead x) * 10;
soulSkill : for each x in enemy field where not dead x, damage x cardinality (y in friendly graveyard);
/*again units in graveyard not all cards*/


unit "Flesh Knight" void level : 5 hp : 70 attack : 40 defense : 0 speed : 3 range : 1 soulPoints : 2
auto : for each x in enemy field, (max hp x) -= 10;
soulSkill : for each x in enemy field, (max hp x) -= 20;

unit "Orb Eyed Monster" void level : 5 hp : 70 attack : 50 defense : 0 speed : 3 range : 1 soulPoints : 2
counter : select x in enemy field x in range self and dead x then {damage x temporary attack self;}
soulSkill : for each x in friendly field, (permanent speed x) := 0;


