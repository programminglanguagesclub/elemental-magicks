



unit "Spectre" spirit level : 2 hp : 40 attack : 0 defense : 0 speed : 1 range : 1 soulPoints : 2
auto : for each x in enemy field where hp x < max hp x / 4, (hp x) := 0;
soulSkill : for each x in enemy field where hp x < max hp x / 4, (hp x) := 0;



unit "Greater Succubus" spirit level : 3 hp : 60 attack : 0 defense : 0 speed : 1 range : 3 soulPoints : 2
auto : for each x in enemy field where engagement x > 0, damage x temporary attack self;
soulSkill : for each x in enemy field where engagement x > 0, damage x 40;




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




