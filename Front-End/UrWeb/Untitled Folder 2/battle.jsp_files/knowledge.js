/* I may want to have the spheres pulsate (slowly change shades?) for the opponent when they level up their spheres. Perhaps could have the thoughts do this as well. I could also have the thoughts be partially filled if you have used up part of it during the set phase.... but I am not sure about that.
*/

//var knowledge = [0,0,0,0,0,0];
//var thoughts = 5;

function incrementKnowledge(index){
	var element = $('#ball' + index);
	--index;
	var currentValue = knowledge[index];
	if(currentValue === 9 || thoughts === 0){return;}
	element.removeClass("value-"+currentValue).addClass("value-"+(currentValue+1));
	++knowledge[index];
	--thoughts;
	document.getElementById("playerThoughtSpan").innerHTML = thoughts;
}

function decrementKnowledge(index){
	var element = $('#ball' + index);
	--index;
	var currentValue = knowledge[index];
	if(currentValue === 0 || currentValue === gameData.knowledge[0][index]){return;}
	element.removeClass("value-"+currentValue).addClass("value-"+(currentValue-1));
	--knowledge[index];
	++thoughts;
	document.getElementById("playerThoughtSpan").innerHTML = thoughts;
}





var earth = false;
function clickEarth(){
	if(drawPhaseOngoing){
		$("#ball1").toggleClass("ball-earth").toggleClass("ball-earth-selected");
		earth = !earth;
	}
}
var fire = false;
function clickFire(){
	if(drawPhaseOngoing){
		$("#ball2").toggleClass("ball-fire").toggleClass("ball-fire-selected");
		fire = !fire;
	}
}
var water = false;
function clickWater(){
	if(drawPhaseOngoing){
		$("#ball3").toggleClass("ball-water").toggleClass("ball-water-selected");
		water = !water;
	}
}
var air = false;
function clickAir(){
	if(drawPhaseOngoing){
		$("#ball4").toggleClass("ball-air").toggleClass("ball-air-selected");
		air = !air;
	}
}
var spirit = false;
function clickSpirit(){
	if(drawPhaseOngoing){
		$("#ball5").toggleClass("ball-spirit").toggleClass("ball-spirit-selected");
		spirit = !spirit;
	}
}
var voidSchool = false;
function clickVoid(){
	if(drawPhaseOngoing){
		$("#ball6").toggleClass("ball-void").toggleClass("ball-void-selected");
		voidSchool = !voidSchool;
	}
}

var excludeAdditionalSchools = false;

function thoughtClick(){
	if(drawPhaseOngoing){
		$("#playerThoughtFigure").toggleClass("ball-thoughts").toggleClass("ball-thoughts-text").toggleClass("ball-thoughts-selected").toggleClass("ball-thoughts-selected-text");
		excludeAdditionalSchools = !excludeAdditionalSchools;
	}
}

function resetSearchSpecification(){
	earth = false;
	fire = false;
	water = false;
	air = false;
	spirit = false;
	voidSchool = false;
	excludeAdditionalSchools = false;
}


function searchCards(){
	/*$.post("tutorial",{json:JSON.stringify({}),channelToken:sessionStorage.token,channelKey:sessionStorage.userId} ,function(data){
		if(data != null)
		for(var i = 0; i < data.length; ++i){	
			if(data[i].type === undefined){
				console.log("undefined:");
			}
			updateFunctions[data[i].type](data[i]);
		}
	});
	*/
	
	//post([{"type":"RequestPartialCardList",page:private.currentPage}]);
	
	var minLevel = document.getElementById("minLevel").value.trim();
	var maxLevel = document.getElementById("maxLevel").value.trim();
	var minHp = document.getElementById("minHp").value.trim();
	var maxHp = document.getElementById("maxHp").value.trim();
	var minMana = document.getElementById("minMana").value.trim();
	var maxMana = document.getElementById("maxMana").value.trim();
	var minAttack = document.getElementById("minAttack").value.trim();
	var maxAttack = document.getElementById("maxAttack").value.trim();
	var minDefense = document.getElementById("minDefense").value.trim();
	var maxDefense = document.getElementById("maxDefense").value.trim();
	var minAgility = document.getElementById("minAgility").value.trim();
	var maxAgility = document.getElementById("maxAgility").value.trim();
	var minRange = document.getElementById("minRange").value.trim();
	var maxRange = document.getElementById("maxRange").value.trim();
	var minSp = document.getElementById("minSp").value.trim();
	var maxSp = document.getElementById("minSp").value.trim();
	
	var minLevelRequired = minLevel !== "";
	var maxLevelRequired = maxLevel !== "";
	var minHpRequired = minHp !== "";
	var maxHpRequired = maxHp !== "";
	var minManaRequired = minMana !== "";
	var maxManaRequired = maxMana !== "";
	var minAttackRequired = minAttack !== "";
	var maxAttackRequired = maxAttack !== "";
	var minDefenseRequired = minDefense !== "";
	var maxDefenseRequired = maxDefense !== "";
	var minAgilityRequired = minAgility !== "";
	var maxAgilityRequired = maxAgility !== "";
	var minRangeRequired = minRange !== "";
	var maxRangeRequired = maxRange !== "";
	var minSpRequired = minSp !== "";
	var maxSpRequired = maxSp !== "";
	if(!minLevelRequired){minLevel = 0;}
	if(!maxLevelRequired){maxLevel = 0;}
	if(!minHpRequired){minHp = 0;}
	if(!maxHpRequired){maxHp = 0;}
	if(!minManaRequired){minMana = 0;}
	if(!maxManaRequired){maxMana = 0;}
	if(!minAttackRequired){minAttack = 0;}
	if(!maxAttackRequired){maxAttack = 0;}
	if(!minDefenseRequired){minDefense = 0;}
	if(!maxDefenseRequired){maxDefense = 0;}
	if(!minAgilityRequired){minAgility = 0;}
	if(!maxAgilityRequired){maxAgility = 0;}
	if(!minRangeRequired){minRange = 0;}
	if(!maxRangeRequired){maxRange = 0;}
	if(!minSpRequired){minSp = 0;}
	if(!maxSpRequired){maxSp = 0;}
	post({"type":"RequestPartialCardList",page:page,minLevel:{value:minLevel,requirement:minLevelRequired},maxLevel:{value:maxLevel,requirement:maxLevelRequired},
		minHP:{value:minHp,requirement:minHpRequired},maxHP:{value:maxHp,requirement:maxHpRequired},minMana:{value:maxMana,requirement:maxManaRequired},
		maxMana:{value:maxMana,requirement:maxManaRequired},
		minAttack:{value:minAttack,requirement:minAttackRequired},maxAttack:{value:maxAttack,requirement:maxAttackRequired},
		minDefense:{value:minDefense,requirement:minDefenseRequired},maxDefense:{value:maxDefense,requirement:maxDefenseRequired},
		minAgility:{value:minAgility,requirement:minAgilityRequired},maxAgility:{value:maxAgility,requirement:maxAgilityRequired},
		minRange:{value:minRange,requirement:minRangeRequired},
		maxRange:{value:maxRange,requirement:maxRangeRequired},minSP:{value:minSp,requirement:minSpRequired},maxSP:{value:maxSp,requirement:maxSpRequired}
	,excludeAdditionalSchools:excludeAdditionalSchools,earthSchool:earth,fireSchool:fire,waterSchool:water,airSchool:air,spiritSchool:spirit,voidSchool:voidSchool});
	
}

function cardListRight(){
	for(var i = 0; i < 25; ++i){
		$("#cardList"+i).removeClass("selected").addClass("unselected");
	}
	selectedCardListCard = undefined;
	++page;
	searchCards();
}
function cardListLeft(){
	for(var i = 0; i < 25; ++i){
		$("#cardList"+i).removeClass("selected").addClass("unselected");
	}
	selectedCardListCard = undefined;
	--page;
	searchCards();
}

