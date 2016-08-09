


{-
STILL WORKING ON THE GRAMMAR:
OOPS FORGOT ABOUT COST


Skill ::= Executable | Existential | Universal
Executable ::= <<List effect>>
Existential ::=









EXAMPLE SKILL:

exists monster x, monster y from friendly board st hp x < hp y
 selected => hp y += 10; hp x := hp y; then
 all monster z from enemy board st hp z < 10
  hp z -= 5;
 failed => friendly thoughts -= 1;





NOW MY ATTEMPT TO REWRITE ALL SKILLS IN THE GAME:









soul_strike : Skill

soul_strike = action;
 exists monster x in enemy board
  selected => damage x $ attack / 2; then
  execute if hp x < 0 then bury x

devastating_strike : Skill
devastating_strike = action; 2 thoughts;
 exists monster x in enemy board
  selected => damage x $ attack * 2;

scavenger : Skill
scavenger = soul;
 exists dead monster x from enemy board
  selected => bury x

crumbling_tower : Skill
crumbling_tower = auto;
 execute maxhp -= 10

eye_of_clairvoyance : Skill
eye_of_clairvoyance = start;
 all monster x in enemy board
  permanent speed x -= 1

scouting : Skill
scouting = soul;
 all monster x in enemy board
  permanent range x -= 1

minor_healing : Skill
minor_healing = action;
 all monster x in friendly board
  hp x += 20

healing_rain : Skill
healing_rain = soul
 all monster x in friendly board
  hp x += 40

decoy : Skill
decoy = start;
 exists monster x in enemy board st engagement x == 0
  selected => damage self $ attack x; engagement x += 1

















-}
