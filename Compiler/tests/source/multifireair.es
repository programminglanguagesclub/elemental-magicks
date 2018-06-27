/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  0    3         3
    2  0    3         3
    3  1    3         2
    4  0    3         3
    5  0    3         3
    6  1    3         2
    7  0    3         3
    8  0    3         3
    9  0    3         3

*/


unit "Angelic FlameSword Soldier" fire air level : 3 hp : 50 attack : 30 defense : 10 speed : 1 range : 1 soulPoints : 1
start : (temporary speed self) += friendly air;
auto : (permanent attack self) += friendly fire;
soulSkill : cost : 3 (friendly air) += 3; (friendly fire) += 3;

unit "Uriel" fire air level : 6 hp : 120 attack : 70 defense : 15 speed : 5 range : 2 soulPoints : 1
end : for each x in enemy field, damage x temporary level x;
soulSkill : for each x in enemy field, damage x temporary level x;

