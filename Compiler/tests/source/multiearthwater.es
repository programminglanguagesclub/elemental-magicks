unit "Oceanic Earth Elemental" earth water level : 3 hp : 20 attack : 20 defense : 0 speed : 1 range : 2 soulPoints : 2
death : (permanent attack self) -= 20; (max hp self) -= 20; (hp self) := max hp self;
auto : (permanent attack self) += 20; (max hp self) += 20; (hp self) += 20;
/*I should add checks to assignment to make sure the LValue and RValue are not equal, etc!!*/
soulSkill : (earth friendly) := water friendly; (water friendly) := 0;


unit "Amphibian Commander" earth water level : 4 hp : 60 attack : 20 defense : 0 speed : 1 range : 1 soulPoints : 1
auto : for each x in friendly field where dead x, revive x; (engagement x) := 1;
/*
need to make sure we allow revive if and only if dead, but still require explicitly saying in the code, to make it clear that
no other cards are getting their engagement set to 1.

I believe I don't currently do any of this...
*/
soulSkill : for each x in friendly field, (permanent defense x) += 20; (permanent range x) -= 1;




