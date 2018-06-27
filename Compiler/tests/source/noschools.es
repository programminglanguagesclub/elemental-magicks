/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  0    3         3
    2  0    3         3
    3  1    3         2
    4  0    3         3
    5  1    3         2
    6  0    3         3
    7  0    3         3
    8  0    3         3
    9  0    3         3

*/




unit "Goblin Berserker" level : 3 hp : 40 attack : 30 defense : 0 speed : 4 range : 1 soulPoints : 1
soulSkill : for each x in friendly field, (permanent speed x) += temporary level x;


unit "Tank" level : 5 hp : 45 attack : 40 defense : 20 speed : 1 range : 2 soulPoints : 2
soulSkill : for each x in friendly field, (permanent defense x) += 10;









