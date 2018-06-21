unit "Ent Saplasdgsdging" earth level : 3 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 1
soulSkill : select x in friendly field then {(hp x) := temporary attack x;}

unit "Ent Sapling" earth level : 3 hp : 35 attack : 25 defense : 10 speed : 2 range : 1 soulPoints : 1 
auto : (max hp self) += 5; (hp self) += 15;
soulSkill : select x in friendly field then {(hp x) := temporary defense x;}


