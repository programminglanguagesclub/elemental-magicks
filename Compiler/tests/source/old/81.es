unit "Ent Sapling" earth level : 3 hp : 65 attack : 20 defense : 10 speed : 1 range : 1 soulPoints : 1 soulSkill : select x in friendly field then {(hp x) := temporary attack x;}

/*

should not be allowed to target self for soul skills. Maybe shouldn't be allowed to reference self at all actually....

*/
