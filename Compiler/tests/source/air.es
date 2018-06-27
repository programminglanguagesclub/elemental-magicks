
unit "Lightning Crawler" air level : 2 hp : 20 attack : 30 defense : 0 speed : 2 range : 2 soulPoints : 2
start : for each x in enemy field, (hp x) -= temporary speed x;
soulSkill : for each x in friendly field, (hp x) -= temporary speed x; (temporary attack x) += temporary speed x * 5;



unit "Bird of Pride" air level : 3 hp : 50 attack : 30 defense : 0 speed : 3 range : 2 soulPoints : 2
auto : select x in friendly field then {(permanent speed x) += 1;}
soulSkill : (air friendly) += 1; select x in friendly field then {(permanent speed x) += 2;}

unit "Thunder Lord" air level : 3 hp : 60 attack : 20 defense : 10 speed : 3 range : 2 soulPoints : 2
auto : for each x in friendly field, (hp x) -= 5; (temporary speed x) += 1;
soulSkill : for each x in friendly field, (hp x) -= temporary speed x * 5; (permanent speed x) += 1; 
/* I want to be able to say after this:::: for each x in enemy field, increase their speed FIRST and then damage them based on speed; NEED TO augment the syntax for multiple universal effects*/
/*I think I never access the permanent stat of a card as an RValue. I should make this explicit in the syntax*/



unit "Seraph" air level : 4 hp : 50 attack : 20 defense : 0 speed : 3 range : 2 soulPoints : 2
spawn : for each x in friendly field, (permanent defense x) += 10;
soulSkill : for each x in enemy field, (permanent speed x) -= 3;
