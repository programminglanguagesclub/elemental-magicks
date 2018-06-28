/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  0    3         3
    2  0    3         3
    3  1    3         2
    4  0    3         3
    5  0    3         3
    6  0    3         3
    7  0    3         3
    8  0    3         3
    9  0    3         3

*/



unit "Reaper Dragoon" air spirit level : 3 hp : 50 attack : 50 defense : 0 speed : 4 range : 2 soulPoints : 2
counter : condition : hp self < 20 (temporary attack self) += 20; (temporary range self) += 1; (temporary speed self) += 2;
soulSkill : select x in friendly field then {(permanent speed x) += 5;}
