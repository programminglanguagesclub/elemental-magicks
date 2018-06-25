unit "Forest Dragon" earth level : 6 hp : 100 attack : 20 defense : 0 speed : 2 range : 5 soulPoints : 2
start : for each x in friendly field, (temporary defense x) += 10;
auto : for each x in friendly field, (max hp x) += 15; (hp x) += 15;
action : select x in friendly field then {(hp x) := max hp x;}
/*have action that fully heals a target unit... might want to make this not able to target self...?*/
soulSkill : for each x in friendly field, (max hp x) += 15; (hp x) += 15;

unit "Ent Sapling" earth level : 3 hp : 35 attack : 25 defense : 10 speed : 2 range : 1 soulPoints : 1 
auto : (max hp self) += 5; (hp self) += 15;
soulSkill : select x in friendly field then {(max hp x) += 50; (hp x) += 50;}

unit "Lightning Crawler" air level : 2 hp : 20 attack : 30 defense : 0 speed : 2 range : 2 soulPoints : 2
start : for each x in enemy field, (hp x) -= temporary speed x;
soulSkill : for each x in friendly field, (hp x) -= temporary speed x; (temporary attack x) += temporary speed x * 5;

unit "Tank" level : 5 hp : 45 attack : 40 defense : 20 speed : 1 range : 2 soulPoints : 2
soulSkill : for each x in friendly field, (permanent defense x) += 10;

unit "Water Colossus" water level : 7 hp : 70 attack : 40 defense : 10 speed : 1 range : 3 soulPoints : 1
start : for each x in enemy field, (temporary attack x) -= temporary defense self;
end : for each x in enemy field, damage x temporary defense self;
spawn : for each x in enemy field, (temporary range x) -= 1;
soulSkill : for each x in friendly field, (permanent defense x) += max hp x - hp x;

unit "Seraph" air level : 4 hp : 50 attack : 20 defense : 0 speed : 3 range : 2 soulPoints : 2
spawn : for each x in friendly field, (permanent defense x) += 10;
soulSkill : for each x in enemy field, (permanent speed x) -= 3;


unit "Princess of the Void" void level : 2 hp : 40 attack : 0 defense : 0 speed : 2 range : 3 soulPoints : 2
start : for each x in enemy field, (temporary defense x) := 0;
soulSkill : for each x in enemy field, (permanent defense x) := 0;

unit "Amphibian Commander" earth water level : 4 hp : 60 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
auto : for each x in friendly field where dead x, revive x; (engagement x) := 1;
/*
need to make sure we allow revive if and only if dead, but still require explicitly saying in the code, to make it clear that
no other cards are getting their engagement set to 1.

I believe I don't currently do any of this...
*/
soulSkill : for each x in friendly field, (permanent defense x) += 20; (permanent range x) -= 1;


unit "Oceanic Earth Elemental" earth water level : 3 hp : 20 attack : 20 defense : 0 speed : 1 range : 2 soulPoints : 2
death : (permanent attack self) -= 20; (max hp self) -= 20; (hp self) := max hp self;
auto : (permanent attack self) += 20; (max hp self) += 20; (hp self) += 20;
/*I should add checks to assignment to make sure the LValue and RValue are not equal, etc!!*/
soulSkill : (earth friendly) := water friendly; (water friendly) := 0;


unit "Thunder Lord" air level : 3 hp : 60 attack : 20 defense : 10 speed : 3 range : 2 soulPoints : 2
auto : for each x in friendly field, (hp x) -= 5; (temporary speed x) += 1;
soulSkill : for each x in friendly field, (hp x) -= temporary speed x * 5; (permanent speed x) += 1; 
/* I want to be able to say after this:::: for each x in enemy field, increase their speed FIRST and then damage them based on speed; NEED TO augment the syntax for multiple universal effects*/
/*I think I never access the permanent stat of a card as an RValue. I should make this explicit in the syntax*/

unit "Guardian Toad" earth level : 1 hp : 30 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
counter : for each x in friendly field, (max hp x) += 5; (hp x) += 5;
spawn : cost : 1 (earth friendly) += 1; (water friendly) += 1;
soulSkill : cost : 5 select x in friendly field then {(max hp x) += 200; (hp x) := max hp x;}


unit "Bird of Pride" air level : 3 hp : 50 attack : 30 defense : 0 speed : 3 range : 2 soulPoints : 2
auto : select x in friendly field then {(permanent speed x) += 1;}
soulSkill : (air friendly) += 1; select x in friendly field then {(permanent speed x) += 2;}


unit "Reaper Dragoon" air spirit level : 3 hp : 50 attack : 50 defense : 0 speed : 4 range : 2 soulPoints : 2
counter : condition : hp self < 20 (temporary attack self) += 20; (temporary range self) += 1; (temporary speed self) += 2;
soulSkill : select x in friendly field then {(permanent speed x) += 5;}


unit "Fox Wizard" earth level : 2 hp : 40 attack : 0 defense : 0 speed : 2 range : 2 soulPoints : 2
start : (water friendly) -= 1;
action : cost : 1 (water friendly) += 3;
soulSkill : select x in friendly field then {(permanent defense x) += water friendly * 5;}
/*make sure that I am not selecting dead units when not applicable*/
/*note modifying permanent stats causes the same command to be performed on the temporary stats.*/


unit "Orc Militia" spirit level : 3 hp : 30 attack : 30 defense : 20 speed : 3 range : 1 soulPoints : 2
action : cost : 1 select x in enemy field x in range self then{damage x temporary attack self * 2;}
/* do damage not -=hp*/
/*I should make the presence of the "where" keyword consistent*/
soulSkill : cost : 1 select x in friendly field then {(permanent attack x) += 30; (permanent defense x) += 20;}


unit "Earth Warbeast" earth level : 5 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 2
end : (hp self) := max hp self;
auto : (permanent attack self) := 15;
soulSkill : for each x in friendly field, (hp x) := max hp x;

unit "Goblin Berserker" level : 3 hp : 40 attack : 30 defense : 0 speed : 4 range : 1 soulPoints : 1
soulSkill : for each x in friendly field, (permanent speed x) += temporary level x;


unit "Heavily Armored Water Colossus" water level : 7 hp : 100 attack : 50 defense : 30 speed : 1 range : 1 soulPoints : 2
soulSkill : for each x in friendly field, (permanent defense x) += 15; (engagement x) += 1;


unit "Princess of Snakes" earth level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 1
action : select x in enemy field then {(hp x) := 0;}
soulSkill : cost : 7 for each x in enemy field, (hp x) := 0;


unit "Mutant Pig" void level : 3 hp : 60 attack : 10 defense : 0 speed : 3 range : 1 soulPoints : 1
death : (max hp self) -= 10; (hp self) := max hp self;
soulSkill : cost : 1 select x in friendly field then {(permanent attack x) *= 2; (hp x) := max hp x;}

unit "Merfolk Warrior" water level : 3 hp : 50 attack : 30 defense : 10 speed : 2 range : 2 soulPoints : 2
auto : (hp self) += 5;
soulSkill : for each x in friendly field, (engagement x) += 2;, for each x in enemy field, (engagement x) += 2;
/*or friendly field union enemy field*/

unit "Chupacabra" fire level : 5 hp : 60 attack : 0 defense : 0 speed : 5 range : 1 soulPoints : 2
auto : (hp self) += fire friendly * 5; /*I don't allow parentheses so I can specify order of operations yet*/
soulSkill : cost : 1 for each x in friendly field, (permanent attack x) += 5 * fire friendly;


unit "Voidling" void level : 1 hp : 30 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
start : for each x in enemy field, (temporary defense x) -= 5;
soulSkill : for each x in enemy field, (permanent defense x) := 0;, (friendly thoughts) += 1;


unit "God of War" fire level : 9 hp : 135 attack : 70 defense : 0 speed : 3 range : 5 soulPoints : 2
start : for each x in enemy field, (hp x) -= 10; /*switch to damage for all three skills*/
action : for each x in enemy field, (hp x) -= temporary attack self;
soulSkill : for each x in enemy field, (hp x) -= 10;


unit "Spectre" spirit level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
auto : for each x in enemy field where hp x < max hp x / 4, (hp x) := 0;
soulSkill : for each x in enemy field where hp x < max hp x / 4, (hp x) := 0;


unit "Combat Sorceress of the Void" void level : 2 hp : 30 attack : 20 defense : 0 speed : 2 range : 2 soulPoints : 2
action : cost : 2 for each x in enemy field, (hp x) -= temporary attack x;
soulSkill : cost : 2 for each x in enemy field, (hp x) -= temporary attack x;


unit "Corrupted Watchtower" void level : 2 hp : 30 attack : 0 defense : 0 speed : 1 range : 4 soulPoints : 1
start : for each x in enemy field, (permanent speed x) -= 1;
auto : (max hp self) -= 10;
soulSkill : for each x in enemy field, (permanent range x) := 1;


unit "Clown of Horror" void level : 3 hp : 40 attack : 10 defense : 0 speed : 1 range : 2 soulPoints : 2
auto : for each x in enemy field, (permanent attack x) -= 5;
soulSkill : for each x in enemy field, damage x temporary attack x;


unit "Demon Whisperer" void level : 3 hp : 60 attack : 20 defense : 0 speed : 2 range : 2 soulPoints : 1
auto : (friendly thoughts) -= 1;
action : (earth enemy) -= 1; (fire enemy) -= 1; (water enemy) -= 1; (air enemy) -= 1; (spirit enemy) -= 1; (void enemy) -= 1;
soulSkill : (enemy thoughts) -= 2;
/*I need an "all schools" option*/



/*I should do damage here. Also, temporary attack always appears as the RValue. I shouldn't have to say temporary!!!*/

unit "Serpent Swordswoman of the Void" void level : 3 hp : 50 attack : 40 defense : 0 speed : 2 range : 1 soulPoints : 1
action : for each x in enemy field where x in range self, (hp x) := 0;
soulSkill : cost : 6 select x in enemy field dead x then {send x to graveyard;}

/*Need to be able to filter by "in range"*/


unit "Spirit Mage" spirit level : 3 hp : 40 attack : 0 defense : 0 speed : 2 range : 2 soulPoints : 2
auto : (temporary attack self) += cardinality (x in enemy field where dead x) * 20;
soulSkill : for each x in friendly field, (temporary attack x) += cardinality (y in friendly graveyard) * 5;
/*again I only want to take the cardinality of units, not spells, in the graveyard*/


/*soulSkill  : for each x in enemy field, (hp x) -= cardinality (y in friendly field where dead y) * 100;
*/

unit "Zombified Reaper" spirit level : 3 hp : 50 attack : 0 defense : 0 speed : 3 range : 1 soulPoints : 1
auto : (temporary attack self) += cardinality (y in enemy graveyard) * 5; /*want to be able to specify y is a unit, not a spell*/
action : cost : 1 select x in enemy field hp x < temporary attack self then {send x to graveyard;}
soulSkill : for each x in enemy field, damage x 20;


unit "Uriel" fire air level : 6 hp : 120 attack : 70 defense : 15 speed : 5 range : 2 soulPoints : 1
end : for each x in enemy field, damage x temporary level x;
soulSkill : for each x in enemy field, damage x temporary level x;


unit "Greater Succubus" spirit level : 3 hp : 60 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 2
auto : for each x in enemy field where engagement x > 0, damage x temporary attack self;
soulSkill : for each x in enemy field where engagement x > 0, damage x 40;


spell "Fire Bolt" fire level : 3
spawn : select x in enemy field not dead x then {damage x fire friendly * 10;}

spell "Ice Bolt" fire level : 2
spawn : select x in enemy field not dead x then {damage x fire friendly * 5;}

spell "Frozen Cage" water level : 4
spawn : select x in enemy field not dead x then {(engagement x) += fire friendly;}


