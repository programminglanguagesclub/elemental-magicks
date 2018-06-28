/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  1    3         2
    2  0    3         3
    3  1    3         2
    4  1    3         2
    5  0    3         3
    6  0    3         3
    7  0    3         3
    8  0    3         3
    9  0    3         3

*/

unit "Guardian Toad" earth water level : 1 hp : 30 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
counter : for each x in friendly field, (max hp x) += 5; (hp x) += 5;
spawn : cost : 1 (earth friendly) += 1; (water friendly) += 1;
soulSkill : cost : 5 select x in friendly field then {(max hp x) += 200; (hp x) := max hp x;}


unit "Oceanic Earth Elemental" earth water level : 3 hp : 20 attack : 20 defense : 0 speed : 1 range : 2 soulPoints : 2
death : (permanent attack self) -= 20; (max hp self) -= 20; (hp self) := max hp self;
auto : (permanent attack self) += 20; (max hp self) += 20; (hp self) += 20;
/*I should add checks to assignment to make sure the LValue and RValue are not equal, etc!!*/
soulSkill : (earth friendly) := water friendly; (water friendly) := 0;


unit "Amphibian Commander" earth water level : 4 hp : 60 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
auto : for each x in friendly field where dead x, revive x; (engagement x) := 1;
/*
need to make sure we allow revive if and only if dead, but still require explicitly saying in the code, to make it clear that
no other cards are getting their engagement set to 1.

I believe I don't currently do any of this...
*/
soulSkill : for each x in friendly field, (permanent defense x) += 20; (permanent range x) -= 1;




