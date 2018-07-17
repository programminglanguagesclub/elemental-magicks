/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  0    9         9
    2  0    9         9
    3  1    9         8
    4  0    9         9
    5  0    9         9
    6  0    9         9
    7  2    9         7
    8  0    9         9
    9  0    9         9
spells:
       have objective missing
lvl 1  3    3         0
    2  3    3         0
    3  3    3         0
    4  3    3         0
    5  3    3         0
    6  1    3         2
    7  1    3         2
    8  3    3         0
    9  3    3         0


*/

unit "Merfolk Warrior" water level : 3 hp : 50 attack : 30 defense : 10 speed : 2 range : 2 soulPoints : 2
auto : (hp self) += 5;
soulSkill : for each x in friendly field, (engagement x) += 2;, for each x in enemy field, (engagement x) += 2;
/*or friendly field union enemy field*/






unit "Heavily Armored Water Colossus" water level : 7 hp : 100 attack : 50 defense : 30 speed : 1 range : 1 soulPoints : 2
soulSkill : for each x in friendly field, (permanent defense x) += 15; (engagement x) += 1;


unit "Water Colossus" water level : 7 hp : 70 attack : 40 defense : 10 speed : 1 range : 3 soulPoints : 1
start : for each x in enemy field, (temporary attack x) -= temporary defense self;
end : for each x in enemy field, damage x temporary defense self;
spawn : for each x in enemy field, (temporary range x) -= 1;
soulSkill : for each x in friendly field, (permanent defense x) += max hp x - hp x;




spell "Frost" water level : 1
spawn : select x in enemy field then {(temporary attack x) := 0;}

spell "Cold Snap" water level : 1
spawn : select x in enemy field then {(temporary range x) := 0;}


spell "34234sdhsdh" water level : 1
spawn : select x in friendly field then {(permanent range x) := 5;}


spell "Frost Bite" water level : 2
spawn : for each x in enemy field where hp x < 20, (permanent speed x) := 0;

spell "Thermal Transfer" water level : 2
spawn : select x in enemy field then {(permanent speed x) -= 5;} {select y in friendly field then {(permanent speed y) += 5;}}

spell "Numbing Cold" water level : 2
spawn : for each x in enemy field, (temporary attack x) -= 30; (temporary speed x) -= 2;

spell "Frost Bite" water level : 3
spawn : select x in enemy field then {(permanent range x) := 0;}

spell "sdhsdahf" water level : 3
spawn : for each x in friendly field, (permanent range x) := 5;

spell "sdgsdhdsh" water level : 3
spawn : for each x in friendly field, (hp x) := max hp x;


spell "Snow Storm" water level : 4
spawn : for each x in enemy field, (temporary range x) := 0;

spell "Ice Storm" water level : 4
spawn : for each x in enemy field, (temporary attack x) := 0; (temporary speed x) := 0;

spell "Frost Armor" water level : 4
spawn : for each x in friendly field, (permanent defense x) += 10; (temporary defense x) += 50;

spell "Frozen Cage" water level : 5
spawn : select x in enemy field not dead x then {(engagement x) += water friendly;}

spell "Mass Revival" water level : 5
spawn : for each x in friendly field where dead x, revive x;

spell "dsgsdhs" water level : 5
spawn : for each x in enemy field where engagement x > 0, (friendly thoughts) += 1;

/*
spell "sdhsdhs" water level : 6
*/

/*add spells for engaging a row or column, etc*/
/*
spell "sdhshds" water level : 6

spell "dshdshds" water level : 6

spell "ddshsh" water level : 7

spell "sdhsdhsh" water level : 7
*/
spell "sdgsdhgsdh" water level : 7
spawn : for each x in enemy field, (permanent attack x) := base attack x;  (permanent defense x) := base defense x; (permanent speed x) := base speed x; (permanent range x) := base range x; (permanent level x) := base level x;


spell "sjgjsg" water level : 8
spawn : for each x in enemy field where engagement x > 0, (friendly thoughts) += 1; (engagement x) += 5;

spell "Superior Mass Revival" water level : 8
spawn : for each x in friendly field where dead x, revive x; (permanent defense x) += 30;

spell "Shatter" water level : 8
spawn : for each x in enemy field where engagement x > 0, (hp x) := 0;
/*gotta make sure per turn disengagement happens before the spawn skills execute!!*/

spell "dgkgksdg" water level : 9
spawn : for each x in enemy field, (engagement x) += 10;

/*
spell "asdgjsgj" water level : 9
spawn : select x in enemy field, RETURN X TO HAND
*/

spell "gjdsgksg" water level : 9
spawn : for each x in enemy field, (permanent range x) := 0; (permanent speed x) := 0; (permanent attack x) := 0;



















