/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  0    9         9
    2  1    9         8
    3  1    9         8
    4  0    9         9
    5  3    9         6
    6  0    9         9
    7  0    9         9
    8  1    9         8
    9  1    9         8
spells:
       have objective missing
lvl 1  3    3         0
    2  3    3         0
    3  3    3         0
    4  3    3         0
    5  3    3         0
    6  3    3         0
    7  3    3         0
    8  3    3         0
    9  3    3         0


*/

/*THIS UNIT NOT FULLY WORKING IN TYPECHECKER (undefined)*/
unit "Pyromancer" fire level : 2 hp : 30 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 1
action : select x in enemy field then {damage x friendly fire * 5; damage unit to the left of x friendly fire * 5; damage unit to the right of x friendly fire * 5; damage unit behind x friendly fire * 5; damage unit in front of x friendly fire * 5;}
/*I should be able to damage everything in a set like this...*/
soulSkill : cost : 1 select x in enemy field then {damage x friendly fire * 5; damage unit to the left of x friendly fire * 5; damage unit to the right of x friendly fire * 5; damage unit behind x friendly fire * 5; damage unit in front of x friendly fire * 5;}


unit "Initiate Dual Weirding Flame Sword Soldier" fire level : 3 hp : 40 attack : 0 defense : 0 speed : 2 range : 1 soulPoints : 2
auto : (temporary attack self) += 10 * friendly fire;
action : for each x in enemy field where x in range self, damage x temporary attack self;
soulSkill : for each x in enemy field, damage x friendly thoughts * 10;, (friendly thoughts) := 0;
/*Now I want to say friendly thoughts = 0, but I need to make sure all enemy units get hit for the friendly thoughts damage * 10 FIRST*/


/*I think maybe every effect that's in a universal should HAVE TO reference the bound variable*/

unit "Emissary of fire" fire level : 5 hp : 90 attack : 40 defense : 10 speed : 3 range : 1 soulPoints : 1
end : select x in enemy field then {damage x 20; (permanent speed x) -= 1;}
soulSkill : for each x in enemy field, damage x cardinality (y in enemy field where not dead y);

unit "Chupacabra" fire level : 5 hp : 60 attack : 0 defense : 0 speed : 5 range : 1 soulPoints : 2
auto : (hp self) += fire friendly * 5; /*I don't allow parentheses so I can specify order of operations yet*/
soulSkill : cost : 1 for each x in friendly field, (permanent attack x) += 5 * fire friendly;

unit "Infernal Commander" fire level : 5 hp : 40 attack : 50 defense : 0 speed : 3 range : 1 soulPoints : 2
spawn : for each x in enemy field, damage x 10;
death : for each x in enemy field, damage x 20;
soulSkill : cost : 10 for each x in enemy field, (hp x) := 0;

unit "Titanic Infernal Centaur" fire level : 8 hp : 120 attack : 80 defense : 10 speed : 1 range : 2 soulPoints : 1
counter : (permanent speed self) += 1;
action : cost : 1 select x in enemy field x in range self then {damage x temporary attack self * 2;}
soulSkill : for each x in enemy field, damage x friendly fire * 10;, (friendly fire) := 0;
/*the little comma is the only difference between doing 0 damage to every enemy past the first one, and doing fire*10 to all.
I should probably think of a better syntax*/

unit "God of War" fire level : 9 hp : 135 attack : 70 defense : 0 speed : 3 range : 5 soulPoints : 2
start : for each x in enemy field, (hp x) -= 10; /*switch to damage for all three skills*/
action : cost : 4 for each x in enemy field, (hp x) -= temporary attack self;
soulSkill : for each x in enemy field, (hp x) -= 10;


spell "Flare" fire level : 1
spawn : select x in enemy field not dead x then {damage x 20;}

spell "Spark" fire level : 1
spawn : select x in enemy field not dead x then {damage x 10; (friendly fire) += 1;}

spell "Ignition" fire level : 1
spawn : for each x in friendly field, (permanent attack x) += 10; (temporary speed x) += 10; (hp x) -= 10;

spell "Flame Strike" fire level : 2
spawn : select x in enemy field not dead x then {damage x fire friendly * 5;}

spell "Burning Fury" fire level : 2
spawn : for each x in friendly field, (temporary attack x) += hp x; (hp x) := 10;

spell "Inner Fire" fire level : 2
spawn : select x in friendly field then {(permanent attack x) += friendly fire * 10; (permanent speed x) += friendly fire;}

spell "Heat Wave" fire level : 3
spawn : for each x in enemy field, damage x cardinality (y in enemy field where not dead y) * 5; /*if I say not dead x, it should be an error*/

spell "Fire Bolt" fire level : 3
spawn : select x in enemy field not dead x then {damage x fire friendly * 10;}

spell "????" fire level : 3
spawn : select x in enemy field then {damage x 50;}

spell "Incinerate" fire level : 4
spawn : select x in enemy field not dead x then {damage x fire friendly * 20;}

spell "??????" fire level : 4
spawn : for each x in enemy field, damage x max hp x - hp x;

spell "Fireball" fire level : 4
spawn : for each x in enemy field, (hp x) -= 30; select x in enemy field then {damage x 30;}


/*
level 5s: do fire knowledge * 10 to enemy units in positions 1-2-3; 4-5-6; 7-8-9
*/



spell "@?@?@?@" fire level : 6
spawn : for each x in enemy field, damage x 40;

/*
spell "@??@@@@@@" fire level : 6
spawn : select x in enemy field, damage x 40;, if hp x <= 0, damage enemy 1 life point
*/


spell "sgssdjgslkgddsfglkj" fire level : 6
spawn : for each x in enemy field, damage x temporary attack x;

/*
spell "?????????????????????????????????" fire level : 7

damage enemy life points by 1

*/


spell "Greater Fireball" fire level : 7
spawn : for each x in enemy field, (hp x) -= 100; select x in enemy field then {damage x 50;}


spell "Massive Fireball" fire level : 7
spawn : cost : 2 for each x in enemy field, (hp x) -= 150; select x in enemy field then {damage x 50;}




spell "Inferno" fire level : 8
spawn : select x in enemy field then {damage x 400; damage unit to the left of x friendly fire * 250; damage unit to the right of x friendly fire * 250; damage unit behind x friendly fire * 250; damage unit in front of x friendly fire * 250;}

spell "Endless Flames" fire level : 8
spawn : for each x in enemy field, damage x 50;, (friendly thoughts) += cardinality (x in enemy field where not dead x and hp x <= 0);

spell "Mass Immolation" fire level : 8
spawn : for each x in enemy field, (hp x) := 0; (friendly fire) -= cardinality (x in enemy field where not dead x and hp x <= 0);

spell "Fire Tornado" fire level : 9
spawn : for each x in enemy field, damage x 180;

spell "Fire Blast" fire level : 9
spawn : for each x in enemy field, damage x 45;
/*And then damage enemy Life Points by the number of units with hp <= 0*/

spell "Summoning of the Sun" fire level : 9
spawn : cost : 3 for each x in enemy field, (hp x) := 0;





