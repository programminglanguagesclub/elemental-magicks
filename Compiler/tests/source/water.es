
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



spell "Frozen Cage" water level : 4
spawn : select x in enemy field not dead x then {(engagement x) += fire friendly;}
