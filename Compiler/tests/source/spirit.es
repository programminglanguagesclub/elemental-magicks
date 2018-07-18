/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  9    9         0
    2  1    9         8
    3  4    9         5
    4  2    9         7
    5  0    9         9
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


unit "Mechanical Spider" spirit level : 1 hp : 10 attack : 10 defense : 0 speed : 2 range : 1 soulPoints : 1
spawn : (enemy thoughts) -= 1;
death : (friendly thoughts) -= 1;
soulSkill : (enemy thoughts) -= 2;

unit "HOW COULD YOU" spirit level : 1 hp : 20 attack : 0 defense : 0 speed : 5 range : 5 soulPoints : 1
action : cost : 1 select x in enemy field, y in friendly field then {(engagement x) += 1; (engagement y) += 1; (friendly thoughts) += 1;}
soulSkill : (enemy thoughts) -= 1; (spirit friendly) += 1;
/*x and y not dead*/

unit "WHY NOT" spirit level : 1 hp : 10 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
auto : (temporary attack self) += 10 * spirit friendly;
soulSkill : cost : 1 (spirit friendly) += 3;


unit "ARGLE" spirit level : 1 hp : 30 attack : 20 defense : 0 speed : 2 range : 1 soulPoints : 1
death : select x in friendly field then {(permanent attack x) += 20;}
soulSkill : select x in enemy field then {(permanent range x) := 0;}


unit "HASG" spirit level : 1 hp : 20 attack : 20 defense : 0 speed : 2 range : 1 soulPoints : 1
auto : select x in friendly field then {(engagement x) -= 1;}
soulSkill : select x in enemy field then {damage x friendly spirit * 5;}


unit "ASGADG" spirit level : 1 hp : 60 attack : 40 defense : 0 speed : 3 range : 1 soulPoints : 1
auto : (friendly thoughts) -= 1;
soulSkill : (friendly thoughts) := 0; (enemy thoughts) := 0;


unit "asghsdhsdhf" spirit level : 1 hp : 20 attack : 20 defense : 0 speed : 3 range : 2 soulPoints : 2
counter : (permanent attack self) += 10;
death : select x in enemy field then {damage x (temporary attack self);}
soulSkill : select x in enemy field then {damage x temporary attack x; (friendly spirit) += 1;}


unit "ddsagfsdh" spirit level : 1 hp : 10 attack : 10 defense : 0 speed : 3 range : 2 soulPoints : 2
start : for each x in friendly field where engagement x > 0, (temporary defense x) += 20;
soulSkill : for each x in friendly field, (engagement x) := 1; (temporary defense x) += 100;


unit "casdgsdfgzdfh" spirit level : 1 hp : 20 attack : 10 defense : 0 speed : 1 range : 1 soulPoints : 2
end : select x in friendly field then {(permanent range x) += 1;}
soulSkill : for each x in friendly field, (permanent range x) += 1;



/*---------------------------------------------------------------------------*/



unit "Acolyte" spirit level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
auto : select x in friendly field then {(permanent level x) += 1;}
action : select x in enemy field then {(permanent level x) -= 1;}
soulSkill : for each x in enemy field, (permanent level x) -= 1;

unit "Goatborn Necromancer" spirit level : 2 hp : 30 attack : 20 defense : 0 speed : 1 range : 3 soulPoints : 2
auto : (enemy thoughts) -= cardinality (x in enemy field where dead x);
soulSkill : (enemy thoughts) -= cardinality (x in enemy field where dead x);

unit "Soul Keeper" spirit level : 2 hp : 40 attack : 10 defense : 0 speed : 2 range : 2 soulPoints : 2
auto : (friendly thoughts) += cardinality (x in friendly field where dead x);
soulSkill : select x in friendly field dead x then {revive x;} /*is this skill repeated?*/


unit "Spectre" spirit level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
auto : for each x in enemy field where hp x < max hp x / 4, (hp x) := 0;
soulSkill : for each x in enemy field where hp x < max hp x / 4, (hp x) := 0;


unit "Succubus Battle Mage" spirit level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 1
auto : (temporary attack self) += cardinality (x in enemy field where engagement x > 0);
soulSkill : select x in enemy field then {(engagement x) += 5;}

/*

Summoner
Schools: spirit
Level: 2
Hp: 20
Mana: 0
Attack: 10
Defense: 0
Agility: 1
Range: 1
Sp: 1

Action SkillMinor Summoning2 Thoughts
Switch a unit from your hand of level 3 and below with your set position.
Soul SkillMinor Summoning2 Thoughts
Switch a unit from your hand of level 3 and below with your set position.

*/



unit "Greater Succubus" spirit level : 3 hp : 60 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 2
auto : for each x in enemy field where engagement x > 0, damage x temporary attack self;
soulSkill : for each x in enemy field where engagement x > 0, damage x 40;


unit "Lupine Horse" spirit level : 3 hp : 40 attack : 0 defense : 0 speed : 4 range : 1 soulPoints : 1
start : for each x in enemy field where temporary level x <= 2, (temporary speed x) -= 2;
auto : (temporary attack self) += friendly spirit * 10;
soulSkill : (enemy thoughts) -= 1; (friendly spirit) += 1;

unit "Monk Necromancer" spirit level : 3 hp : 60 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
start : (permanent attack self) -= 10;
auto : (permanent attack self) += cardinality (x in friendly field where dead x) * 10;
soulSkill : for each x in enemy field, (permanent attack x) -= cardinality (y in friendly field where dead y) * 20;


/*
unit "Necromancer" spirit level : 3 hp : 50 attack : 30 defense : 0 speed : 2 range : 2 soulPoints : 1
action : 
soulSkill :
*/
/*
switch action to auto
Action SkillSoul Restore
Return the first unit in your graveyard to the first position of your hand.
Soul SkillRedemption5 Thoughts
Return all cards from your graveyard to the beginning of your hand in reverse order.

*/


unit "Shadow Walker" spirit level : 3 hp : 60 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
death : (hp self) += temporary attack self; (permanent attack self) := 0;
auto : (permanent attack self) += 10;
soulSkill : cost : 1 for each x in friendly field, (max hp x) += temporary attack x; (hp x) += temporary attack x;


unit "Orc Militia" spirit level : 3 hp : 30 attack : 30 defense : 20 speed : 3 range : 1 soulPoints : 2
action : cost : 1 select x in enemy field x in range self then{damage x temporary attack self * 2;}

/*I should make the presence of the "where" keyword consistent*/
soulSkill : cost : 1 select x in friendly field then {(permanent attack x) += 30; (permanent defense x) += 20;}


unit "Spirit Mage" spirit level : 3 hp : 40 attack : 0 defense : 0 speed : 2 range : 2 soulPoints : 2
auto : (temporary attack self) += cardinality (x in enemy field where dead x) * 20;
soulSkill : for each x in friendly field, (temporary attack x) += cardinality (y in friendly graveyard) * 5;
/*again I only want to take the cardinality of units, not spells, in the graveyard*/


/*soulSkill  : for each x in enemy field, (hp x) -= cardinality (y in friendly field where dead y) * 100;
*/

unit "Twinheaded Zombie Hound" spirit level : 3 hp : 50 attack : 40 defense : 0 speed : 2 range : 1 soulPoints : 1
soulSkill : (spirit friendly) += cardinality (x in friendly graveyard) / 3;

/*
Soul SkillCall of the Dead
Increase your spirit knowledge by [number of units in your graveyard / 3].
Death SkillRestore1 Mana
Send a copy of this card from your hand to your graveyard, causing this unit to fully heal.
*/


unit "Zombified Reaper" spirit level : 3 hp : 50 attack : 0 defense : 0 speed : 3 range : 1 soulPoints : 1
auto : (temporary attack self) += cardinality (y in enemy graveyard) * 5; /*want to be able to specify y is a unit, not a spell*/
action : cost : 1 select x in enemy field hp x < temporary attack self then {send x to graveyard;}
soulSkill : for each x in enemy field, damage x 20;


unit "Orc Conquerer" spirit level : 4 hp : 60 attack : 50 defense : 0 speed : 3 range : 1 soulPoints : 2
action : cost : 2 for each x in enemy field where x in range self, damage x temporary attack self;
action : cost : 1 select x in enemy field not dead x then {damage x temporary attack self * 2;}
soulSkill : for each x in friendly field, (temporary attack x) += 10; (temporary defense x) += 10; (permanent speed x) += 1;


unit "Sword Dancer" spirit level : 4 hp : 50 attack : 40 defense : 0 speed : 5 range : 1 soulPoints : 1
auto : (permanent speed self) -= 1; (permanent range self) += 1;
action : for each x in enemy field where engagement x > 0, damage x temporary attack self;
soulSkill : (friendly spirit) += 1; select x in enemy field then {(engagement x) += 1;} 




unit "Death Ogre" spirit level : 6 hp : 80 attack : 60 defense : 10 speed : 2 range : 1 soulPoints : 1
action : select x in enemy field x in range self then {damage x temporary attack self * 2;}
soulSkill : select x in enemy field then {damage x friendly spirit * 20;}


unit "Skeletal Colossus" spirit level : 6 hp : 80 attack : 80 defense : 0 speed : 3 range : 1 soulPoints : 2
/*death : refresh ... need to add this to compiler*/
soulSkill : for each x in friendly field, (permanent defense x) += 10;


unit "Titanic Heavily Armoured Robot Soldier" spirit level : 6 hp : 90 attack : 70 defense : 10 speed : 1 range : 1 soulPoints : 1
auto : (permanent defense self) += 5;
soulSkill : for each x in friendly field, (permanent defense x) += 15;


unit "Tyrant" spirit level : 7 hp : 90 attack : 0 defense : 0 speed : 3 range : 2 soulPoints : 2
auto : (temporary attack self) += cardinality (x in enemy graveyard) * 10;
action : condition : cardinality (x in friendly graveyard) >= 10 select x in enemy field not dead x then {send x to graveyard;}
soulSkill : select x in friendly field then {(permanent attack x) += cardinality (y in friendly graveyard) * 10;}
/*again should be units in grave, not cards*/

unit "Undead Wyrm" spirit level : 7 hp : 100 attack : 50 defense : 0 speed : 2 range : 3 soulPoints : 1
action : (enemy thoughts) -= 1; select x in enemy field x in range self then {damage x temporary attack self;}
soulSkill : cost : 2 (enemy thoughts) -= 5;

unit "Titanic Death Knight" spirit level : 8 hp : 80 attack : 50 defense : 20 speed : 3 range : 1 soulPoints : 1
action : cost : 2 for each x in enemy field, damage x cardinality (y in enemy graveyard) * 5;
soulSkill : select x in friendly field then {(permanent defense x) += cardinality (y in friendly graveyard) * 5;}
/*again units not cards*/




