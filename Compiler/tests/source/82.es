unit "Forest Dragon" earth level : 3 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 1
soulSkill : for each x in friendly field, (max hp x) += 20; (hp x) += 20;

unit "Ent Sapling" earth level : 3 hp : 35 attack : 25 defense : 10 speed : 2 range : 1 soulPoints : 1 
auto : (max hp self) += 5; (hp self) += 15;
soulSkill : select x in friendly field then {(max hp x) += 50; (hp x) += 50;}


