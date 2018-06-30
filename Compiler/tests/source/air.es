/*
Version 1.0 objectives:

units:
       have objective missing
lvl 1  0    9         9
    2  1    9         8
    3  2    9         7
    4  1    9         8
    5  0    9         9
    6  0    9         9
    7  0    9         9
    8  0    9         9
    9  0    9         9
spells:
       have objective missing
lvl 1  0    3         3
    2  0    3         3
    3  0    3         3
    4  0    3         3
    5  0    3         3
    6  0    3         3
    7  0    3         3
    8  0    3         3
    9  0    3         3


*/

unit "Keeper of the Night" air level : 1 hp : 30 attack : 10 defense : 0 speed : 2 range : 1 soulPoints : 1
start : select x in friendly field then {(temporary speed x) += 1;}
soulSkill : for each x in enemy field, (engagement x) += 1;

unit "Bird Soldier" air level : 2 hp : 40 attack : 30 defense : 0 speed : 1 range : 1 soulPoints : 2
auto : for each x in friendly field, (permanent defense x) += temporary speed x;
soulSkill : for each x in friendly field, (permanent defense x) += temporary speed x;


unit "Haste Sorceress" air level : 2 hp : 30 attack : 10 defense : 0 speed : 4 range : 3 soulPoints : 1
start : for each x in friendly field, (permanent speed x) += 1;
action : for each x in friendly field, (permanent attack x) += temporary speed x;
soulSkill : for each x in friendly field, (permanent speed x) += 3;



unit "Lightning Crawler" air level : 2 hp : 20 attack : 30 defense : 0 speed : 2 range : 2 soulPoints : 2
start : for each x in enemy field, (hp x) -= temporary speed x;
soulSkill : for each x in friendly field, (hp x) -= temporary speed x; (temporary attack x) += temporary speed x * 5;


/*

change to earth-air, and have buff max hp as well.

Royal Songbird
Schools: air
Level: 2
Hp: 30
Mana: 5
Attack: 10
Defense: 0
Agility: 1
Range: 3
Sp: 1
Action SkillSong of Healing
All friendly units heal 5 times their agility.
Soul SkillSong of Summoning2 Thoughts
Target air unit from your hand with no additional schools and of level 3 or lower is sent to target position on your field.

*/

unit "Bird of Pride" air level : 3 hp : 50 attack : 30 defense : 0 speed : 3 range : 2 soulPoints : 2
auto : select x in friendly field then {(permanent speed x) += 1;}
soulSkill : (air friendly) += 1; select x in friendly field then {(permanent speed x) += 2;}


unit "Combat Eagle" air level : 3 hp : 50 attack : 30 defense : 0 speed : 5 range : 2 soulPoints : 2
action : select x in enemy field then {damage x temporary speed self * 10;}
soulSkill : select x in enemy field then {damage x air friendly * 10;}

unit "Dracoavian Skirmisher" air level : 3 hp : 50 attack : 30 defense : 0 speed : 2 range : 1 soulPoints : 1
start : (temporary defense self) += (temporary speed self) * cardinality (x in friendly field where not dead x);
soulSkill : for x in friendly field, (temporary defense x) += (temporary speed x) * cardinality (y in friendly field where not dead y);

unit "Pegasus" air level : 3 hp : 40 attack : 20 defense : 0 speed : 2 range : 2 soulPoints : 2
/*
Start SkillMounted Combat
For this turn, the units immediately to the left and right of this unit get +1 agility, the unit immediately in front of this unit gets +10 defense, and the unit immediately behind this unit gets +30 attack.
*/
soulSkill : for each x in friendly field, (temporary attack x) += 10; (temporary defense x) += 10; (temporary speed x) += 10;




unit "Thunder Lord" air level : 3 hp : 60 attack : 20 defense : 10 speed : 3 range : 2 soulPoints : 2
auto : for each x in friendly field, (hp x) -= 5; (temporary speed x) += 1;
soulSkill : for each x in friendly field, (hp x) -= temporary speed x * 5; (permanent speed x) += 1; 
/* I want to be able to say after this:::: for each x in enemy field, increase their speed FIRST and then damage them based on speed; NEED TO augment the syntax for multiple universal effects*/
/*I think I never access the permanent stat of a card as an RValue. I should make this explicit in the syntax*/



unit "Seraph" air level : 4 hp : 50 attack : 20 defense : 0 speed : 3 range : 2 soulPoints : 2
spawn : for each x in friendly field, (permanent defense x) += 10;
soulSkill : for each x in enemy field, (permanent speed x) -= 3;
