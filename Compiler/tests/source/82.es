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
/*removing hp instead of dealing damage, both because I want that and because idk if I have damage implemented yet*/

unit "Tank" level : 5 hp : 45 attack : 40 defense : 20 speed : 1 range : 2 soulPoints : 2
soulSkill : for each x in friendly field, (permanent defense x) += 10;

unit "Water Colossus" water level : 7 hp : 70 attack : 40 defense : 20 speed : 1 range : 3 soulPoints : 1
start : for each x in enemy field, (temporary attack x) -= temporary defense self;
end : for each x in enemy field, (hp x) -= temporary defense self; /*change to dealing damage*/
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

/*
unit "Orc Militia" spirit level : 3 hp : 30 attack : 30 defense : 20 speed : 3 range : 1 soulPoints : 2
action : do damage attack * 2 to target enemy unit in range (costs 1 thought)....
*/


unit "Earth Warbeast" earth level : 5 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 2
end : (hp self) := max hp self;
auto : (permanent attack self) := 15;
soulSkill : for each x in friendly field, (hp x) := max hp x;

unit "Goblin Berserker" level : 3 hp : 40 attack : 30 defense : 0 speed : 4 range : 1 soulPoints : 1
soulSkill : for each x in friendly field, (permanent speed x) += temporary level x;


unit "Heavily Armored Water Colossus" water level : 7 hp : 100 attack : 80 defense : 30 speed : 1 range : 1 soulPoints : 2
soulSkill : for each x in friendly field, (permanent defense x) += 15; (engagement x) += 1;


unit "Princess of Snakes" earth level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 1
action : select x in enemy field then {(hp x) := 0;}
soulSkill : cost : 7 for each x in enemy field, (hp x) := 0;


unit "Mutant Pig" void level : 3 hp : 60 attack : 10 defense : 0 speed : 3 range : 1 soulPoints : 1
death : (max hp self) -= 10; (hp self) := max hp self;
soulSkill : cost : 1 select x in friendly field then {(permanent attack x) *= 2; (hp x) := max hp x;}

unit "Merfolk Warrior" water level : 3 hp : 50 attack : 30 defense : 10 speed : 2 range : 2 soulPoints : 2
auto : (hp self) += 5;
soulSkill : for each x in friendly field union enemy field, (engagement x) += 2;
/*I should also be able to use two for each statements, instead of having a union.*/




