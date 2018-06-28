/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  0    3         3
    2  0    3         3
    3  3    3         0
    4  0    3         3
    5  2    3         1
    6  0    3         3
    7  0    3         3
    8  0    3         3
    9  0    3         3

*/


unit "Axeman" level : 3 hp : 50 attack : 30 defense : 0 speed : 2 range : 1 soulPoints : 2
soulSkill : select x in enemy field then {damage x 50;}

unit "Goblin Berserker" level : 3 hp : 40 attack : 30 defense : 0 speed : 4 range : 1 soulPoints : 1
soulSkill : for each x in friendly field, (permanent speed x) += temporary level x;

unit "Rogue Assassin" level : 3 hp : 30 attack : 30 defense : 0 speed : 2 range : 3 soulPoints : 2
action : cost : 2 select x in enemy field then {(hp x) := 0;}
soulSkill : cost : 2 select x in enemy field then {(hp x) := 0;}

unit "Guardian Angel" level : 5 hp : 50 attack : 30 defense : 10 speed : 2 range : 3 soulPoints : 1
spawn : select x in friendly field dead x then {revive x;}
soulSkill : for each x in friendly field where dead x, revive x;


unit "Tank" level : 5 hp : 45 attack : 40 defense : 20 speed : 1 range : 2 soulPoints : 2
soulSkill : for each x in friendly field, (permanent defense x) += 10;









