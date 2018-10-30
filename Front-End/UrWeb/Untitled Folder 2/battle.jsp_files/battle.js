

var updates = (function(){
	var privateScope = {
		updates:[]
	};
	var publicScope = {
		registerUpdate:function(arg){
			privateScope.updates.push(arg);
		},
		executeUpdates:function(){
			for(var i =0, length = privateScope.updates.length; i < length; ++i){
				privateScope.updates[i]();
			}
		}
	}; 
	return publicScope;
}());

function gameViewSize(scale){
	function min(a,b){
		if(a > b)
			return b;
		return a;
	}
	return min($(window).width() * scale / (1000/10), $(window).height() * scale / 100*(10/9));
}



$( document ).ready(function() {
	$("body").on("mouseenter", ".graveyardImage", function(event) {displayDetailsGraveyard(0,parseInt($(event.target).attr("index"))); }); 
	$("body").on("mouseleave", ".graveyardImage", function(event) {hideDetails(); });
	
	$("body").on("mouseenter", ".enemyGraveyardImage", function(event) {displayDetailsGraveyard(1,parseInt($(event.target).attr("index"))); }); 
	$("body").on("mouseleave", ".enemyGraveyardImage", function(event) {hideDetails(); }); 
	
	$("body").on("mouseenter", ".handImage", function(event) {displayDetailsHand(0,parseInt($(event.target).attr("index"))); }); 
	$("body").on("mouseleave", ".handImage", function(event) {hideDetails(); });
	
	$("body").on("mouseenter", ".enemyHandImage", function(event) {displayDetailsHand(1,parseInt($(event.target).attr("index"))); }); 
	$("body").on("mouseleave", ".enemyHandImage", function(event) {hideDetails(); });
	
	$("body").on("mouseenter", ".cardListImage", function(event) {displayDetailsCardList(parseInt($(event.target).attr("index"))); });
	$("body").on("mouseleave", ".cardListImage", function(event) {hideDetails(); });
	$("body").on("mouseleave", ".fieldCard", function(event) {hideDetails(); });
	$("body").on("mouseleave", ".setCard", function(event) {hideDetails(); });
	
	$("body").on("mouseenter", "#playerSoulImage0", function(event) {displayDetailsPlayerSoul(0); });
	$("body").on("mouseleave", "#playerSoulImage0", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulImage1", function(event) {displayDetailsPlayerSoul(1); });
	$("body").on("mouseleave", "#playerSoulImage1", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulImage2", function(event) {displayDetailsPlayerSoul(2); });
	$("body").on("mouseleave", "#playerSoulImage2", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulImage3", function(event) {displayDetailsPlayerSoul(3); });
	$("body").on("mouseleave", "#playerSoulImage3", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulImage4", function(event) {displayDetailsPlayerSoul(4); });
	$("body").on("mouseleave", "#playerSoulImage4", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulImage0", function(event) {displayDetailsEnemySoul(0); });
	$("body").on("mouseleave", "#enemySoulImage0", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulImage1", function(event) {displayDetailsEnemySoul(1); });
	$("body").on("mouseleave", "#enemySoulImage1", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulImage2", function(event) {displayDetailsEnemySoul(2); });
	$("body").on("mouseleave", "#enemySoulImage2", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulImage3", function(event) {displayDetailsEnemySoul(3); });
	$("body").on("mouseleave", "#enemySoulImage3", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulImage4", function(event) {displayDetailsEnemySoul(4); });
	$("body").on("mouseleave", "#enemySoulImage4", function(event) {hideDetails(); });
	
	
	
	
	$("body").on("mouseenter", "#playerSoulBack0", function(event) {displayDetailsPlayerSoul(0); });
	$("body").on("mouseleave", "#playerSoulBack0", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulBack1", function(event) {displayDetailsPlayerSoul(1); });
	$("body").on("mouseleave", "#playerSoulBack1", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulBack2", function(event) {displayDetailsPlayerSoul(2); });
	$("body").on("mouseleave", "#playerSoulBack2", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulBack3", function(event) {displayDetailsPlayerSoul(3); });
	$("body").on("mouseleave", "#playerSoulBack3", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#playerSoulBack4", function(event) {displayDetailsPlayerSoul(4); });
	$("body").on("mouseleave", "#playerSoulBack4", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulBack0", function(event) {displayDetailsEnemySoul(0); });
	$("body").on("mouseleave", "#enemySoulBack0", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulBack1", function(event) {displayDetailsEnemySoul(1); });
	$("body").on("mouseleave", "#enemySoulBack1", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulBack2", function(event) {displayDetailsEnemySoul(2); });
	$("body").on("mouseleave", "#enemySoulBack2", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulBack3", function(event) {displayDetailsEnemySoul(3); });
	$("body").on("mouseleave", "#enemySoulBack3", function(event) {hideDetails(); });
	$("body").on("mouseenter", "#enemySoulBack4", function(event) {displayDetailsEnemySoul(4); });
	$("body").on("mouseleave", "#enemySoulBack4", function(event) {hideDetails(); });
	
	
	
	
	
	
	
	
	
	
	
	// not doing anything with this line yet....
	$("body").on("click", ".handImage", function(event) {phase.clickPlayerHand(parseInt($(event.target).attr("index")));});
	$("body").on("click", ".enemyHandImage", function(event) {phase.clickEnemyHand(parseInt($(event.target).attr("index")));});
	
	////playerSoulImage0
	
	$("body").on("click", ".cardListImage", function(event) {console.log(event.target); selectCardListCard(parseInt($(event.target).attr("index"))); });
	
	
	updates.registerUpdate(function(){
		$("#cardname").css("font-size",gameViewSize(1.3));
	});
	updates.registerUpdate(function(){
		$('.cardStat').each(function(i, obj) {
			$(obj).css("font-size",gameViewSize(1.4));
		});
	});
	updates.registerUpdate(function(){
		$('.cardSkill').each(function(i, obj) {
			$(obj).css("font-size",gameViewSize(1.4));
		});
	});
	updates.registerUpdate(function(){
		$('.cardSkillDescription').each(function(i, obj) {
			$(obj).css("font-size",gameViewSize(1.4));
		});
	});
	updates.registerUpdate(function(){
		$(".ball-earth").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
		$(".ball-earth-selected").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".ball-fire").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".ball-water").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".ball-air").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".ball-spirit").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".ball-void").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".ball-thoughts").each(function(index,element) {
			//$(element).css("font-size",gameViewSize(1.4)+"px");
			$(element).css("font-size",gameViewSize(.8)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".thought-span").each(function(index,element) {
			$(element).css("font-size",gameViewSize(4)+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".phaseStuff").each(function(index,element) {
			$(element).css("font-size",gameViewSize(2.0)-2+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".clockDisplay").each(function(index,element) {
			///$(element).css("font-size",gameViewSize(5)-2+"px");
			$(element).css("font-size",gameViewSize(4)-2+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".turnNumber").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)+"px");
		});
	});
	updates.registerUpdate(function(){
		
		$(".animateStatChangeText").each(function(index,element) {
			$(element).css("font-size",gameViewSize(1.5)+"px");
		});
		
		
		$(".playerName").each(function(index,element) {
			$(element).css("font-size",gameViewSize(1.5)-2+"px");
		});
		$(".opponentName").each(function(index,element) {
			$(element).css("font-size",gameViewSize(1.5)-2+"px");
		});
		$(".opponentRating").each(function(index,element) {
			$(element).css("font-size",gameViewSize(2)-2+"px");
		});
		$(".playerRating").each(function(index,element) {
			$(element).css("font-size",gameViewSize(2)-2+"px");
		});
		$(".playerScore").each(function(index,element) {
			$(element).css("font-size",gameViewSize(2)-2+"px");
		});
		$(".opponentScore").each(function(index,element) {
			$(element).css("font-size",gameViewSize(2)-2+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".input").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)-2+"px");
		});
		$(".input2").each(function(index,element) {
			$(element).css("font-size",gameViewSize(3)-2+"px");
		});
		$(".myButton").each(function(index,element) {
			//$(element).css("font-size",gameViewSize(1.4)-2+"px");
			$(element).css("font-size",gameViewSize(3)-2+"px");
		});
		$(".myButtonSmall").each(function(index,element) {
			$(element).css("font-size",gameViewSize(1.4)-2+"px");
		});
		$(".myButtonResign").each(function(index,element) {
			$(element).css("font-size",gameViewSize(1.4)-2+"px");
		});
	});
	updates.registerUpdate(function(){
		$(".cardList").each(function(index,element) {
			$(element).css("font-size",gameViewSize(6)+"px");
		});
	});
	/*
	updates.registerUpdate(function(){
		$("#playerCardsDrawn").each(function(index,element) {
			$(element).css("font-size",gameViewSize(15)+"px");
		});
	});
	updates.registerUpdate(function(){
		$("#enemyCardsDrawn").each(function(index,element) {
			$(element).css("font-size",gameViewSize(15)+"px");
		});
	});
	*/
	updates.registerUpdate(function(){
		recenter();
	});
	updates.executeUpdates();
	var res;
	window.onresize=function() {
	    if (res){clearTimeout(res)};
	    res = setTimeout(function(){
	    	updates.executeUpdates();
	    },100);
	};
	// THIS MIGHT NOT WORK WITH ALL TIMEOUTS THERE MIGHT BE RACE CONDITIONS ETC ETC.
	
	
	
	
	raj();
});


function connect(){
	updateFunctions.Token({token:sessionStorage.token});
	
}

function displayCardSchools(card){
	console.log(card);
	/// apparently cards not in the card list already have the school mapped....
	var schoolMap = ["earth","fire","water","air","spirit","void"];//{0:"earth",1:"fire",2:"water",3:"air",4:"spirit",5:"void"};
	var SSS = "";
	for(var i = 0; i < card.school.length-1; ++i){
		SSS += schoolMap[card.school[i]] + ",";
	}
	if(card.school.length > 0){
		for(var i = card.school.length-1; i < card.school.length; ++i){
			SSS += schoolMap[card.school[i]];
		}
	}
	else{
		SSS = "none";
	}
	document.getElementById("schools").innerHTML = SSS;
}

function displayCardStatsCardList(card){
	console.log(card);
	displayCardSchools(card);
	document.getElementById("level").innerHTML = renderCardStats.hp(card.level,card.level,card.level);
	document.getElementById("hp").innerHTML = renderCardStats.hp(card.hp,card.hp,card.hp);
	document.getElementById("mana").innerHTML = renderCardStats.hp(card.mana,card.mana,card.mana);
	document.getElementById("attack").innerHTML = renderCardStats.hp(card.attack,card.attack,card.attack);
	document.getElementById("defense").innerHTML = renderCardStats.hp(card.defense,card.defense,card.defense);
	document.getElementById("agility").innerHTML = renderCardStats.hp(card.agility,card.agility,card.agility);
	document.getElementById("range").innerHTML = renderCardStats.hp(card.range,card.range,card.range);
	document.getElementById("sp").innerHTML = renderCardStats.hp(card.LP,card.LP,card.LP);
	
	document.getElementById("cardname").innerHTML = card.name;
	$(document.getElementById("cardareaimage")).attr("src",card.imagehtml);	
	
	//document.getElementById("engagementWrapper").style.display = "none";  // make these display in all the correct places later.
	//document.getElementById("statusWrapper").style.display = "none";
}


function displayCardStats(card){
	displayCardSchools(card);
	
	document.getElementById("level").innerHTML = renderCardStats.hp(card.currentLevel,card.modifiedLevel,card.baseLevel);
	document.getElementById("hp").innerHTML = renderCardStats.hp(card.currentHP,card.modifiedMaxHP,card.baseMaxHP);
	document.getElementById("mana").innerHTML = renderCardStats.hp(card.currentMana,card.currentMaxMana/*refactoring to modifiedMaxMana*/,card.baseMaxMana);
	document.getElementById("attack").innerHTML = renderCardStats.hp(card.currentAttack,card.modifiedAttack,card.baseAttack);
	document.getElementById("defense").innerHTML = renderCardStats.hp(card.currentDefense,card.modifiedDefense,card.baseDefense);
	document.getElementById("agility").innerHTML = renderCardStats.hp(card.currentAgility,card.modifiedAgility,card.baseAgility);
	document.getElementById("range").innerHTML = renderCardStats.hp(card.currentRange,card.modifiedRange,card.baseRange);
	document.getElementById("sp").innerHTML = renderCardStats.hp(card.LP,card.LP,card.LP);
	
	document.getElementById("cardname").innerHTML = card.name;
	$(document.getElementById("cardareaimage")).attr("src",card.imagehtml);	
}


function displayCardSkills(card){
	
var generated = "";
	
	var index = 0;
	
	if(card.actionSkills !== undefined){
		for(var i = 0; i < card.actionSkills.length; ++i){
			
			$("#cardSkillType"+(i+index)).html("Action Skill");
			$("#cardSkillName"+(i+index)).html(card.actionSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.actionSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.actionSkills[i]);
			/*
			generated += "<div><font class = 'cardSkill'  color='red' style = 'float:left;'>Action Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.actionSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.actionSkillsCost[i]+"</font></div>";
			generated += "<div><font class = 'cardSkillDescription' color='white'>"+card.actionSkills[i]+"</font></div>";
			 */
		}
		index += card.actionSkills.length;
	}
	if(card.autoSkills !== undefined){
		for(var i = 0; i < card.autoSkills.length; ++i){
	
			$("#cardSkillType"+(i+index)).html("Auto Skill");
			$("#cardSkillName"+(i+index)).html(card.autoSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.autoSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.autoSkills[i]);
			
			/*
			generated += "<font class = 'cardSkill'  color='red' style = 'float:left;'>Auto Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.autoSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.autoSkillsCost[i]+"</font><br>";
			generated += "<font class = 'cardSkillDescription' color='white'>"+card.autoSkills[i]+"</font>";
*/
		}
		index += card.autoSkills.length;
	}
	if(card.openSkills !== undefined){
		for(var i = 0; i < card.openSkills.length; ++i){
			
			$("#cardSkillType"+(i+index)).html("Set Skill");
			$("#cardSkillName"+(i+index)).html(card.openSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.openSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.openSkills[i]);
			
			
			/*
			generated += "<font class = 'cardSkill'  color='red' style = 'float:left;'>Open Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.openSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.openSkillsCost[i]+"</font><br>";
			generated += "<font class = 'cardSkillDescription' color='white'>"+card.openSkills[i]+"</font>";
*/
		}
		index += card.openSkills.length;
	}
	if(card.counterSkills !== undefined){
		for(var i = 0; i < card.counterSkills.length; ++i){
			
			$("#cardSkillType"+(i+index)).html("Counter Skill");
			$("#cardSkillName"+(i+index)).html(card.counterSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.counterSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.counterSkills[i]);
			
			/*
			generated += "<font class = 'cardSkill'  color='red' style = 'float:left;'>Counter Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.counterSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.counterSkillsCost[i]+"</font><br>";
			generated += "<font class = 'cardSkillDescription' color='white'>"+card.counterSkills[i]+"</font>";
		*/
		}
		index += card.counterSkills.length;
	}
	if(card.startSkills !== undefined){
		for(var i = 0; i < card.startSkills.length; ++i){
	
			$("#cardSkillType"+(i+index)).html("Start Skill");
			$("#cardSkillName"+(i+index)).html(card.startSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.startSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.startSkills[i]);
			
			
			/*
			generated += "<font class = 'cardSkill'  color='red' style = 'float:left;'>Start Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.startSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.startSkillsCost[i]+"</font><br>";
			generated += "<font class = 'cardSkillDescription' color='white'>"+card.startSkills[i]+"</font>";
*/
		}
		index += card.startSkills.length;
	}
	if(card.endSkills !== undefined){
		for(var i = 0; i < card.endSkills.length; ++i){
	
			
			$("#cardSkillType"+(i+index)).html("End Skill");
			$("#cardSkillName"+(i+index)).html(card.endSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.endSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.endSkills[i]);
			
			/*
			generated += "<font class = 'cardSkill'  color='red' style = 'float:left;'>End Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.endSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.endSkillsCost[i]+"</font><br>";
			generated += "<font class = 'cardSkillDescription' color='white'>"+card.endSkills[i]+"</font>";
*/
		}
		index += card.endSkills.length;
	}
	if(card.closeSkills !== undefined){
		for(var i = 0; i < card.closeSkills.length; ++i){
			
			$("#cardSkillType"+(i+index)).html("Death Skill");
			$("#cardSkillName"+(i+index)).html(card.closeSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.closeSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.closeSkills[i]);
			
			/*
			generated += "<font class = 'cardSkill'  color='red' style = 'float:left;'>Close Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.closeSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.closeSkillsCost[i]+"</font><br>";
			generated += "<font class = 'cardSkillDescription' color='white'>"+card.closeSkills[i]+"</font>";
			*/
		}
		index += card.closeSkills.length;
	}
	if(card.soulSkills !== undefined){
		for(var i = 0; i < card.soulSkills.length; ++i){
			
			$("#cardSkillType"+(i+index)).html("Soul Skill");
			$("#cardSkillName"+(i+index)).html(card.soulSkillsName[i]);
			$("#cardSkillCost"+(i+index)).html(card.soulSkillsCost[i]);
			$("#cardSkillDescription"+(i+index)).html(card.soulSkills[i]);
			/*
			generated += "<font class = 'cardSkill'  color='red' style = 'float:left;'>Soul Skill </font>";
			generated += "<font class = 'cardSkill' color = 'green'>"+card.soulSkillsName[i]+"</font>";
			generated += "<font class = 'cardSkill' color = lightblue style = 'float:right;'>"+card.soulSkillsCost[i]+"</font><br>";
			generated += "<font class = 'cardSkillDescription' color='white'>"+card.soulSkills[i]+"</font>";
*/
		}
		index += card.soulSkills.length;
	}
	var maximumNumberOfSkills = 7;
	for(var i = index; i < maximumNumberOfSkills; ++i){
		$("#cardSkillType"+(i)).html("");
		$("#cardSkillName"+(i)).html("");
		$("#cardSkillCost"+(i)).html("");
		$("#cardSkillDescription"+(i)).html("");
	}
	
	
	//document.getElementById("skillArea").innerHTML = generated;
	
	
	// NEED TO ORGANIZE ALL OF THIS AND MAKE DISPLAY DETAILS ALWAYS WORK...
	// ALSO MAKE SURE THAT THE SKILL TEXT IS RESIZING BECAUSE IT ISN'T CURRENTLY.
	
	
	
	
}

function displayCardDetails(card){
	displayCardStats(card);
	displayCardSkills(card);
	document.getElementById("nondisplaycard").style.display = 'none';
	document.getElementById("cardcardcard").style.display = 'block';
}

function displayDetailsField(player,index){
	displayCardDetails(gameData.fieldData[player][index]);
}


function displayDetailsSet(player){
	displayCardDetails(gameData.setData[player]);
}

function displayDetailsPlayerSoul(index){
	displayCardDetails(gameData.soulData[0][index]);
}

function displayDetailsEnemySoul(index){
	displayCardDetails(gameData.soulData[1][index]);
}

function displayDetailsHand(player, index){
	displayCardDetails(gameData.handData[player][index]);
}

function displayDetailsGraveyard(player, index){
	displayCardDetails(gameData.graveyardData[player][index]);
}

function displayDetailsCardList(index){
	//displayCardDetails(cards[index]); // cards is a global currently I guess.
	var card = cards[index];
	
	console.log(card);
	
	displayCardStatsCardList(card);
	displayCardSkills(card);
	document.getElementById("nondisplaycard").style.display = 'none';
	document.getElementById("cardcardcard").style.display = 'block';
}

function hideDetails(){
	document.getElementById("cardcardcard").style.display = 'none';
	document.getElementById("nondisplaycard").style.display = 'block';
}

//$("body").on("mouseenter", ".displaySomeDetails", function(event) {myDisplaySomeDetails(this); }); // event.target is inconsistent so use this.
//$("body").on("mouseleave", ".displayDetails, .displaySomeDetails", function() {document.getElementById("displayDetailsWrapper").style.visibility = 'hidden'; } );


function selectPlayerField(){
	document.getElementById("container5player").style.display = "none";
	document.getElementById("container5playerhand").style.display = "none";
	document.getElementById("container4ccc").style.display = "block";
	$(document.getElementById("playerfieldbutton")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
	$(document.getElementById("playergraveyardbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	$(document.getElementById("playerhandbutton2")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
}

function selectPlayerGraveyard(){
	document.getElementById("container5player").style.display = "block";
	document.getElementById("container5playerhand").style.display = "none";
	document.getElementById("container4ccc").style.display = "none";
	$(document.getElementById("playergraveyardbutton")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
	$(document.getElementById("playerhandbutton2")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	$(document.getElementById("playerfieldbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
}

function selectCardList(){
	document.getElementById("container5playercardlist").style.display = "block";
	document.getElementById("container5playerhand").style.display = "none";
	document.getElementById("container4ccc").style.display = "none";
	$(document.getElementById("playerhandbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	//$(document.getElementById("playergraveyardbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	$(document.getElementById("playerCardListButton")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
}

function selectPlayerHand(){
	
	document.getElementById("container5playerhand").style.display = "block";
	document.getElementById("container5playercardlist").style.display = "none";
	document.getElementById("container5player").style.display = "none";
	document.getElementById("container4ccc").style.display = "none";
	$(document.getElementById("playerhandbutton2")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
	$(document.getElementById("playerhandbutton")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
	if(!drawPhaseOngoing){
		$(document.getElementById("playergraveyardbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	}
	$(document.getElementById("playerCardListButton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	
	
	
	$(document.getElementById("playerfieldbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
}

function selectEnemyHand(){
	document.getElementById("container5enemyhand").style.display = "block";
	document.getElementById("containerEnemyGraveyard").style.display = "none";
	document.getElementById("container4gggccc").style.display = "none";
	$(document.getElementById("enemygraveyardbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	$(document.getElementById("enemyfieldbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	$(document.getElementById("enemyhandbutton")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
	
	
}
function selectEnemyGraveyard(){
	document.getElementById("container5enemyhand").style.display = "none";
	document.getElementById("containerEnemyGraveyard").style.display = "block";
	document.getElementById("container4gggccc").style.display = "none";
	$(document.getElementById("enemygraveyardbutton")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
	$(document.getElementById("enemyfieldbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	$(document.getElementById("enemyhandbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
}
function selectEnemyField(){
	document.getElementById("container5enemyhand").style.display = "none";
	document.getElementById("containerEnemyGraveyard").style.display = "none";
	document.getElementById("container4gggccc").style.display = "block";
	$(document.getElementById("enemygraveyardbutton")).addClass("displayAreaUnselected").removeClass("displayAreaSelected");
	$(document.getElementById("enemyfieldbutton")).addClass("displayAreaSelected").removeClass("displayAreaUnselected");
	$(document.getElementById("enemyhandbutton")).addClass("displayAreaUnSelected").removeClass("displayAreaSelected");
}

function displayCard(){
	document.getElementById("cardarea").style.visibility = "visible";
}
function hideCard(){
	document.getElementById("cardarea").style.visibility = "hidden";
}


function clickedHp(){
	$(".input").each(function(index,element) {
		$(element).css("font-size",gameViewSize(10)+"px");
	});
}

function recenter(){
	$("#centerall").css("margin-left",($(window).width() - gameViewSize(100))/2*100/$(window).width()+"%");
}

function fx(o)
{
	var x = gameViewSize(15);
	var y = x/2;
	var $o=$(o);
      $o.html($o.text().replace(/([\S])/g,'<span>$1</span>'));
      $o.css('position','relative');
      $('span',$o).stop().css({opacity:0,fontSize:x,
                               top:function(i){return ((i%2)?1:-1);},
                               left:function(i){return ((i%2)?1:-1);}
                               
                              }).animate({},500).animate({opacity:1,fontSize:y,top:30,left:0},500).animate({opacity:0},500);

}