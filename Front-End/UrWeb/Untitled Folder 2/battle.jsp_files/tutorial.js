"use strict";
var currentChallenger = "";
var playerToBool = {0:true,1:false};
/*

If you click the login button before the html resets, you get a 404 error on the main page.


*/
var utility = {

};
	var googleChannel =
	{
			token:"aa",
			user:"aaa", // I might not need this, user is stored in another way in the jsp.
			userId:"aaaa",
			channel:null,
			onOpened: function()
			{
				gameInterface.print("You have successfully connected to the game.");
			},
			onMessage: function(msg) {
				if(msg.data !== "")
				{// JSON.parse("") => unexpected end of input
					//dealwithpost(JSON.parse(msg.data));
					var data = JSON.parse(msg.data);
					updateManager.addPayload(data);
					/*for(var i = 0; i < data.length; ++i){	
						if(data[i].type === undefined){console.log(data[i]);}
						
						updateFunctions[data[i].type](data[i]);
					}*/
				}
			},
			onError: function(err) {
				alert(err);
			},
			onClose: function() { gameInterface.print("You have been disconnected."); }
	};	
function myDisplayDetails(arg) {
	
	console.log("hERHHEHREHREHRHEHRHE!!!!!!!!!!!!!!!");
	console.error("myDisplayDetailsmyDisplayDetailsmyDisplayDetailsmyDisplayDetailsmyDisplayDetailsmyDisplayDetailsmyDisplayDetailsmyDisplayDetails");
	
	
	var container; var card; var tempstring = "";
	if(arg.id.substring(0,9) == "enemyHand") {
		var cardIndex = arg.id.substring(9);
		container = gameData.handData[1];
		card = container[cardIndex];
	}
	else if(arg.id.substring(0,14) == "enemyGraveyard") {
		var cardIndex = arg.id.substring(14);
		container = gameData.graveyardData[1];
		card = container[cardIndex];
	}
	else if(arg.id.substring(0,10) == "playerHand") {	
		var cardIndex = arg.id.substring(10);
		container = gameData.handData[0];
		card = container[cardIndex];		
	}
	else if(arg.id.substring(0,15) == "playerGraveyard") {	
		var cardIndex = arg.id.substring(15);
		container = gameData.graveyardData[0];
		card = container[cardIndex];		
	}
	else if(arg.id.substring(0,9) == "enemySoul") {					
		var cardIndex = arg.id.substring(9);
		container = gameData.soulData[1];
		card = container[cardIndex];
	}
	else if(arg.id.substring(0,10) == "playerSoul") {			
		var cardIndex = arg.id.substring(10);
		container = gameData.soulData[0];
		card = container[cardIndex];
	}
	// THE JQUERY .hasClass selector does not work. I will switch back to manipulating ids.
	else if(arg.id.substring(0,6) =='player') {
		if(gameData.fieldData[0][arg.id.substring(6)].name != "no card") {
			container = gameData.fieldData[0];
			card = container[arg.id.substring(6)];
		}
	}
	else if(arg.id.substring(0,5) == 'enemy') {
		if(gameData.fieldData[1][arg.id.substring(5)].name != "no card") {
			container = gameData.fieldData[1];
			card = container[arg.id.substring(5)];
		}
	}
	if(card === undefined) return;
	//actionSkills, autoSkills, openSkills, counterSkills, startSkills, endSkills, soulSkills, closeSkills;
	for(var q = 0; q < card.actionSkills.length; ++q) {
		tempstring += "<b>" + card.actionSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements + card.actionSkills[q].description  + "<br />";
	}
	for(var q = 0; q < card.autoSkills.length; ++q) {
		tempstring += "<b>" + card.autoSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements  + card.autoSkills[q].description  + "<br />";
	}
	for(var q = 0; q < card.openSkills.length; ++q) {
		tempstring += "<b>" + card.openSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements  + card.openSkills[q].description  + "<br />";
	}
	for(var q = 0; q < card.counterSkills.length; ++q)
	{
		tempstring += "<b>" + card.counterSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements + card.counterSkills[q].description + "<br />";
	}
	for(var q = 0; q < card.startSkills.length; ++q) {
		tempstring += "<b>" + card.startSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements + card.startSkills[q].description + "<br />";
	}
	for(var q = 0; q < card.endSkills.length; ++q) {
		tempstring += "<b>" + card.endSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements +  card.endSkills[q].description  + "<br />";
	}
	for(var q = 0; q < card.soulSkills.length; ++q) {
		tempstring += "<b>" + card.soulSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements + card.soulSkills[q].description + "<br />";
	}
	for(var q = 0; q < card.closeSkills.length; ++q)
	{
		tempstring += "<b>" + card.closeSkills[q].name + ": </b>";
		var temprequirements = "<i>description:</i> ";
		tempstring += temprequirements + card.closeSkills[q].description + "<br />";
	}
	//$(document.getElementById("displayAbilities")).html("<strong>Card Abilities</strong><br />" + tempstring);
	document.getElementById("displayAbilities").innerHTML = "<font size=1>" + "<strong>Card Abilities</strong><br />" + tempstring + "</font>";
	$(document.getElementById("displayImage")).html("<img src=" + card.displayhtml + "/>");
	
	console.log("ABOUT TO SET DISPLAYED CARD");
	console.log(card);
	
	gameInterface.setDisplayedCard(card);
	document.getElementById("displayDetailsWrapper").style.visibility = 'visible';
}









function generateSkillDetails(container,skilltype)
{
	var retVal = "";
	if(container.length > 0)
		retVal += "<center>"+skilltype+"</center><center>------------</center>"
	for(var i = 0, j = container.length; i < j; ++i)
	{
		retVal += "<b>" + container[i].name + ": </b>";
		retVal += container[i].description + "<br />";
	}
	return retVal;
}
function myDisplaySomeDetails(arg) {
	var card = cards[arg.id.substring(8)];
	var tempstring = "";
	tempstring += generateSkillDetails(card.actionSkills,"Action Skills");
	tempstring += generateSkillDetails(card.autoSkills,"Auto Skills");
	tempstring += generateSkillDetails(card.openSkills,"Open Skills");
	tempstring += generateSkillDetails(card.counterSkills,"Counter Skills");
	tempstring += generateSkillDetails(card.startSkills,"Start Skills");
	tempstring += generateSkillDetails(card.endSkills,"End Skills");
	tempstring += generateSkillDetails(card.soulSkills,"Soul Skills");
	tempstring += generateSkillDetails(card.closeSkills,"Close Skills");
	document.getElementById("displayAbilities").innerHTML = "<font size=1>" + "<strong>Card Abilities</strong><br />" + tempstring + "</font>";
	$("#displayImage").html("<img src=" + card.displayhtml + "/>");
	gameInterface.setMinimallyDisplayedCard(card);
	document.getElementById("displayDetailsWrapper").style.visibility = 'visible';
}


$("body").on("mouseenter", ".displayDetails", function(event) {myDisplayDetails(this); });
$("body").on("mouseenter", ".displaySomeDetails", function(event) {myDisplaySomeDetails(this); }); // event.target is inconsistent so use this.
$("body").on("mouseleave", ".displayDetails, .displaySomeDetails", function() {document.getElementById("displayDetailsWrapper").style.visibility = 'hidden'; } );




	// THIS IS NOT BEING CALLED AFTER THE CARD IS SET, REQUIRING THE USER TO CLICK ON DISPLAY HAND TO SEE THE UPDATE
// FURTHER, THE CARD IN THE SET POSITION DOES NOT RESPOND TO MOUSING OVER IT.

// I can solve this problem by switching to field view when the user plays a card.  


// THIS DOES NOT DEAL WITH SOUL DATA, ETC THAT HAS NOT BEEN LOADED CORRECTLY YET.......

function generateHandGraveHTML(arg,arg2)// the arg specifies whether we are asked for the hand or the graveyard, arg2 stores either "handgrave" or "enemyhandgrave"
// this may be changed eventually, perhaps to storing the text handgrave or enemyhandgrave in the handData[0],handData[1] objects.
{
	var numCards = arg.length;
	var str = ""; 
	var numrows = Math.floor(numCards/3);//handData[0].length/3); // replaced arg.length with numCards.
	var leftover = numCards - 3*numrows; //handData[0].length - 3*numrows;
		for(var i = 0; i < numrows; ++i)
		{	
			str += "<tr>";
			for(var j = 0; j < 3; ++j)
			{
				var couldbeundefined;
				if(arg[j+3*i] == undefined)
					couldbeundefined = "images/cardnotloaded.png"; // eventually switch to jpg maybe
				else
					couldbeundefined = arg[j+3*i].imagehtml;
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				var temp;
				temp = "\'" + arg2 + (j+3*i).toString() + "\'";	
				//temp = "\'" + (j+3*i).toString() + "\'";	
				
				temp1 =  j+3*i;
				
				if(arg2 === 'playerHand')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickPlayerHand(" +temp1 + ");\"" + ">";
				}
				else if(arg2 === 'enemyHand')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickEnemyHand(" +temp1 + ");\"" + ">";
				}
				else if(arg2 === 'playerGraveyard')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickPlayerGraveyard(" +temp1 + ");\"" + ">";
				}
				else if(arg2 === 'enemyGraveyard')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickEnemyGraveyard(" +temp1 + ");\"" + ">";
				}
				else
				{
					console.log("generateHandGraveHTML error invalid container: " + arg2);
				}
					
					
					str += "<a href=\"#\"><img src=" + couldbeundefined + "></a>";//handData[0][j+3*i].imagehtml + "\"></a></td></tr>";
				str += "</td>";
			}			
			str += "</tr>";	
		}
		if(leftover > 0)
		{
		str += "<tr>";  		
			for(var k = 0; k < leftover; ++k)
			{
				var couldbeundefined;
				if(arg[k+3*numrows] == undefined)
					couldbeundefined = "images/cardnotloaded.png";// eventually switch to jpg maybe
				else
					couldbeundefined = arg[k+3*numrows].imagehtml;
				temp = "\'" + arg2 + (numrows*3 + k).toString() + "\'";
				//temp = "\'" + (numrows*3 + k).toString() + "\'";
				var temp1 = numrows*3 + k;
				
				if(arg2 === 'playerHand')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickPlayerHand(" +temp1 + ");\"" + ">";
				}
				else if(arg2 === 'enemyHand')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickEnemyHand(" +temp1+ ");\"" + ">";
				}
				else if(arg2 === 'playerGraveyard')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickPlayerGraveyard(" +temp1 + ");\"" + ">";
				}
				else if(arg2 === 'enemyGraveyard')
				{
					str += "<td id="+temp +"class = \"handgravecard unselected displayDetails\""+ " onclick=\"phase.clickEnemyGraveyard(" +temp + ");\"" + ">";
				}
				else
				{
					console.log("generateHandGraveHTML error invalid container: " + arg2);
				}
				
				
					if(arg2 == "handgrave")
					{
						str += "<a href=\"#\"><img src=" + couldbeundefined + "></a>";
					}
					else	
					{
						str += "<a href=\"#\"><img src=" + couldbeundefined + "></a>";
					}
				str += "</td>";
			}			
		str += "</tr>";
		}	
	return str; 
}
var cards =[];
var page = 1; // might be a duplicate.
var selectedCardListCard;

function selectCardListCard(arg){
	for(var i = 0; i < 25; ++i){
		$("#cardList"+i).removeClass("selected").addClass("unselected");
	}
	if(selectedCardListCard != arg){
		$("#cardList"+arg).removeClass("unselected").addClass("selected");
		selectedCardListCard = arg;
	}
	else{
		selectedCardListCard = undefined;
	}
}

function selectHandCard(arg){
	if(!(phase.selectedPositions[2].contains(arg))){
		$("#hand"+arg).removeClass("unselected").addClass("selected");
		if(phase.getName() === "set"){
			if(phase.selectedPositions[2].length > 0){
				$("#hand"+phase.selectedPositions[2][0]).removeClass("selected").addClass("unselected");
				phase.selectedPositions[2].shift();
			}
		}
		phase.selectedPositions[2].push(arg);
	}
	else{
		$("#hand"+arg).removeClass("selected").addClass("unselected");
		phase.selectedPositions[2].remove(arg);
	}
}

function selectEnemyHandCard(arg){
	if(!(phase.selectedPositions[3].contains(arg))){
		$("#enemyhand"+arg).removeClass("unselected").addClass("selected");
		phase.selectedPositions[3].push(arg);
	}
	else{
		$("#enemyhand"+arg).removeClass("selected").addClass("unselected");
		phase.selectedPositions[3].remove(arg);
	}
}


function drawCard(){
	/*
	int soulIndex;
//	String cardName;

	long id; // NOT IMP
	*/ // ignore soulIndex for now.
	if(phase.selectedPositions[6][0] !== undefined){
		post({"type":"DrawCard",id:cards[selectedCardListCard].id,soulIndex:phase.selectedPositions[6][0]});
	}
	else{
		post({"type":"DrawCard",id:cards[selectedCardListCard].id,soulIndex:-1});
	}
}


function generateCardListHTML()
{	
	// This should be moved to gameInterface.
	var retVal = "";
	var rows = Math.floor(cards.length/3);
	for(var i = 0;i<rows;++i)
	{
		retVal += "<tr>";
		for(var j = 0; j < 3; ++j)
		{ // I shouldn't be setting the name and the soul card location at once.. only soul card location when I click on a soul card, and only name when I click on a card
			retVal += "<td id = 'cardList"+(i*3+j)+"' class = 'displaySomeDetails' onclick='gameInterface.selectCard("+(3*i+j)+")' >" +
					"<div class = 'unselected'><img src= "+cards[3*i+j].imagehtml+" /></div></td>";
		}
		retVal += "</tr>";
	}
	if(rows*3 != cards.length)
	{
		retVal += "<tr>";
		for(var k = rows*3; k < cards.length; ++k)
		{
			retVal += "<td id = 'cardList"+(k)+"' class = 'unselected displaySomeDetails' onclick='gameInterface.selectCard("+k+")' >" +
					"<div class = 'unselected'><img src= "+cards[k].imagehtml+" /></div></td>";
		}
		retVal += "</tr>";
	}
	return retVal;	
}
	function CardData() // constructor
	{ 
		this.dead = false;			
	}
	function NoCardData() // constructor
	{
		this.name = "no card";
	}	
	Array.prototype.contains = function(obj) {
	var i = this.length;
	while (i--) {
	if (this[i] == obj) {
	return true;
	}
	}
	return false;
	}
	Array.prototype.remove = function() {
	var what, a = arguments, L = a.length, ax;
	while (L && this.length) {
	what = a[--L];
	while ((ax = this.indexOf(what)) !== -1) {
	this.splice(ax, 1);
	}
	}
	return this;
	};
	function dealwithdeck(slot,card,kluji)
	{
			var data = card.data;
			slot.name = data.name;
			slot.actionSkills = data.actionSkills; slot.actionSkillsName = data.actionSkillsName; slot.actionSkillsCost = data.actionSkillsCost;
			slot.autoSkills = data.autoSkills; slot.autoSkillsName = data.autoSkillsName; slot.autoSkillsCost = data.autoSkillsCost;
			slot.openSkills = data.openSkills; slot.openSkillsName = data.openSkillsName; slot.openSkillsCost = data.openSkillsCost;
			slot.counterSkills = data.counterSkills; slot.counterSkillsName = data.counterSkillsName; slot.counterSkillsCost = data.counterSkillsCost;
			slot.startSkills = data.startSkills; slot.startSkillsName = data.startSkillsName; slot.startSkillsCost = data.startSkillsCost;
			slot.endSkills = data.endSkills; slot.endSkillsName = data.endSkillsName; slot.endSkillsCost = data.endSkillsCost;
			slot.soulSkills = data.soulSkills; slot.soulSkillsName = data.soulSkillsName; slot.soulSkillsCost = data.soulSkillsCost;
			slot.closeSkills = data.closeSkills; slot.closeSkillsName = data.closeSkillsName; slot.closeSkillsCost = data.closeSkillsCost;
			
			
			slot.imagehtml = "/images/" + data.imagehtml + ".jpg"; slot.displayhtml = "/images/" + data.imagehtml + "Display.jpg";
			var stats = data.stats;
			//temp = array[player][i];
			// could organize these into a better data structure to allow easier enumeration.
			slot.currentAttack =  slot.modifiedAttack = slot.baseAttack = stats.attack;		
			slot.currentDefense = slot.modifiedDefense = slot.baseDefense = stats.defense;
			slot.currentAgility = slot.modifiedAgility = slot.baseAgility = stats.agility;
			slot.currentRange = slot.modifiedRange = slot.baseRange = stats.range;
			slot.LP = slot.maxLP = stats.lp;
			slot.currentMana = slot.baseMana = 0;
			slot.currentMaxMana = slot.baseMaxMana = stats.mana;
			slot.baseLevel = slot.modifiedLevel = slot.currentLevel = stats.level; // Modified from slot.level to slot.currentLevel.

			slot.currentHP = slot.currentMaxHP = slot.modifiedMaxHP = slot.baseMaxHP = stats.hp;
			slot.school = stats.schoolsOfThought;

			slot.engaged = 0; // dead also false, etc.
			
			if(!card.hand) // just refresh enemy and player slots...
			{
				if(kluji) {
					gameInterface.populateEnemySoul(card.index,gameData.soulData[1][card.index].imagehtml);
					gameData.LP[1] += stats.lp;
					gameInterface.updateLifePoints();
				}
				else {
					gameInterface.populatePlayerSoul(card.index,gameData.soulData[0][card.index].imagehtml);
					gameData.LP[0] += stats.lp;
					gameInterface.updateLifePoints();				
				}
			}
	}
	function post(arg,servlet){	
		if(servlet == undefined) {
			servlet = "tutorial";
		}
		$.post(servlet,{json:JSON.stringify(arg),channelToken:sessionStorage.token,channelKey:sessionStorage.userId} ,function(data){
			
			console.error("outputting data");
			console.error(data);
			
			if(data != null){
				updateManager.addPayload(data);
			}
			/*for(var i = 0; i < data.length; ++i){	
				if(data[i].type === undefined){
					console.log("undefined:");
				}
				updateFunctions[data[i].type](data[i]);
			}*/
		});
	}
	function swapArrays(setA, setB, indexA, indexB){
	    setB.splice(indexB,1,setA.splice(indexA, 1, setB[indexB]));
	}
	function swapArray(set,value,index){
	     return set.splice(index, 1, value);
	}
	
	
	
	
	
	
	
	
	
	
	var drawPhaseOngoing = true;
	updates.executeUpdates();
	function endDraw(){
		
		console.log("in end draw");
		
		document.getElementById("cardSearchArea").style.display = "none";
		document.getElementById("cardSearchAreaOther").style.display = "block";
		
		document.getElementById("ball1").title = "Earth knowledge; Playing an earth card requires your earth knowledge not be below its level";
		document.getElementById("ball2").title = "Fire knowledge; Playing a fire card requires your fire knowledge not be below its level";
		document.getElementById("ball3").title = "Water knowledge; Playing a water card requires your water knowledge not be below its level";
		document.getElementById("ball4").title = "Air knowledge; Playing an air card requires your air knowledge not be below its level";
		document.getElementById("ball5").title = "Spirit knowledge; Playing a spirit card requires your spirit knowledge not be below its level";
		document.getElementById("ball6").title = "Void knowledge; Playing a void card requires your void knowledge not be below its level";
		document.getElementById("playerThoughtFigure").title = "Thoughts; Playing a card consumes its level in thoughts. You will get this amount back if the card dies and leaves the playing area during the removal phase, provided it left normally and was not removed by a skill.";
		
		
		
		
		document.getElementById("enemyIcons").style.visibility = "visible";
		
		
		//document.getElementById("cardSearchAreaOther").style.visibility = "hidden";
				
		document.getElementById("cardSearchButton").style.visibility = "hidden";
		document.getElementById("enemyResources").style.display = "block";
		$("#playerThoughtFigure").removeClass("ball-thoughts-text").removeClass("ball-thoughts-selected-text").removeClass("ball-thoughts-selected").addClass("ball-thoughts");
		document.getElementById("playerThoughtSpan").innerHTML = 5;
		
		
		
		$("#ball1").addClass("ball-earth").removeClass("ball-earth-selected");
		$("#ball2").addClass("ball-fire").removeClass("ball-fire-selected");
		$("#ball3").addClass("ball-water").removeClass("ball-water-selected");
		$("#ball4").addClass("ball-air").removeClass("ball-air-selected");
		$("#ball5").addClass("ball-spirit").removeClass("ball-spirit-selected");
		$("#ball6").addClass("ball-void").removeClass("ball-void-selected");
		for(var i = 1; i < 7; ++i){
			$("#ball"+i).addClass("value-0");
		}
		document.getElementById("container57777").style.display = "none";
		document.getElementById("playIcons").style.display = "block";
		
		$(".plus-button").each(function(index,element) {
			$(element).css("display","block");
		});
		$(".minus-button").each(function(index,element) {
			$(element).css("display","block");
		});
		
		selectPlayerHand();
		
		drawPhaseOngoing = false;
		console.log("leaving end draw");
	}
	
	
	function AttemptToReturnToLobby(){
		
		// IF I USE THE BIG POST->UpdateManager OBJECT I RUN INTO ISSUES WITH PAYLOAD TRACKING.
		
		// I SHOULD NOT DO IT THAT WAY I GUESS.
		
		//post([{"type":"GetOutOfGameSearch","userId":sessionStorage.userId}],"index"); 		
		$.post("index",{json:JSON.stringify([{"type":"GetOutOfGameSearch","userId":sessionStorage.userId}])} ,function(data){
			if(data != null) {// JQuery replaces empty arrays with null
				for(var i = 0; i < data.length; ++i){
					updateFunctions[data[i].type](data[i]);
				}
			}
		});
	}
	
	
	
	
	
	
	
	
	var typeChecker = function(){
	
		function extract(x, list) {
		    var step1 = (x+"");
		    var step4 = [];
		    for (var i = 0; i < list.length; ++i) {  	
		    	var hh = list[i]+"."+constructRegex(constructCharacters(convertToValues(validCalls.get())));
				var step2 = step1.match(new RegExp(hh, "g"));
				if(step2 === null){
					step2 = [];		
				}
				for(var j = 0; j < step2.length; ++j){
		   			var step3 = step2[j];
		    		step4.push(step3.split("."));
				}
		    }
		    return step4;
		}
		function checkCorrectCalls(x){
		    var list = [];
			var calls = validCalls.get();
		    for(var key in calls){
				list.push(key);
		    }
		    var callsList = extract(x, list);
		    var incorrectCalls = [];
		    for (var i = 0; i < callsList.length; ++i){
				if (contains(calls[callsList[i][0]],callsList[i][1])) {
				}
				else {
					incorrectCalls.push(callsList[i]);
				}
		    }
		    return incorrectCalls;
		}
		function contains(arr, val){
		    if(arr.indexOf(val) === -1){
				return false;
		    }
		    return true;
		}
		var validCalls = function(){
			var objects = {};
			var publicScope = {
				"registerObject":function(objectName){
					//objects[objectName] = Object.keys(window[objectName]);
					objects[objectName] = [];
					for(var k in window[objectName]){
						objects[objectName].push(k);
						//console.log(k);
					}
					//console.log(objects[objectName]);
				},
				"get":function(){
					return objects;
				}
			};
			return publicScope;
		}();
		function initializeStaticAnalyzer(){
			validCalls.registerObject("gameInterface");
			validCalls.registerObject("gameData");
			//validCalls.registerObject("updateFunctions");
			//validCalls.registerObject("phase");
			//validCalls.registerObject("gameData");
			//validCalls.registerObject("clock");
			//validCalls.registerObject("userInput");
		}
		function check(){
			initializeStaticAnalyzer();
			var objects = validCalls.get();
			for(var key7 in objects){
				for(var func in window[key7]){
					var invalidCalls = checkCorrectCalls(window[key7][func]);
					if(invalidCalls.length > 0){
						console.error("Invalid function calls in " + key7 +"."+ func + ":");
						console.error(JSON.stringify(invalidCalls));
						console.error(window[key7][func]);		
					}
				}					
			}
		}
		function convertToValues(objs){
			var arr = [];
			for(var key in objs){
				arr.push(objs[key])	
			}
			return arr;
		}
		function convertToKeys(objs){
			var arr = [];
			for(var key in objs){
				arr.push(key)	
			}
			return arr;
		}
		function constructRegex(usedCharacters){
		    // assumes that no characters used will have to be escaped.
		    if(usedCharacters.length === 0){
		return "()"; // not sure....
		    }
		    var retVal = "([a-z]|[A-Z]|"+usedCharacters[0];
		    for(var i = 1; i < usedCharacters.length; ++i){
		retVal += "|" + usedCharacters[i];
		    }
		    retVal += ")*";
		    return retVal;
		}
		function constructCharacters(objs){
		    var usedCharacters = [];
		    for(var j = 0; j < objs.length; ++j){
				var obj = objs[j];
				for(var key in obj){
		    		var field = obj[key];
		    		for(var i = 0; i < field.length; ++i){
						if(!contains(usedCharacters,field[i])){
			    			usedCharacters.push(field[i]);
						}
		    		}
				}
		    }
		    return usedCharacters;
		}
		return {run:function(){check()}};
	}();
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	var skillCurrently = false;
	var moveCurrently = false;
	
	
	var soulPoints = [[0,0],[0,0],[0,0],[0,0],[0,0]]; // right: last SP to damage; first index of subarrays is current SP on card, right is max SP on card.

	function populate(index, SP){
		for(var i = 0; i < 4; ++i){
			document.getElementById("LP"+index+"-"+i+"-wrapper").style.display = "none";
		}
		document.getElementById("LP"+index+"-"+SP+"-wrapper").style.display = "inline";
		soulPoints[index] = [SP,SP];
	}


	function enemyPopulate(index, SP){
		for(var i = 0; i < 4; ++i){
			document.getElementById("enemyLP"+index+"-"+i+"-wrapper").style.display = "none";
		}
		document.getElementById("enemyLP"+index+"-"+SP+"-wrapper").style.display = "inline";
		soulPoints[index] = [SP,SP];
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	







	$("#messageArea").html("");
	var textAnimations = [];
	var lock = false;
	var firstLine = true;
	var lines = 0;
	function animateText(data){
		var text = data.val;
		var color = data.color;
		if(lock){
			textAnimations.push({val:text,color:color});
			return;
		}
	lock = true;
		function _animateText(val,firstCharacter,element){
			element.animate({visibility:"visible"}, 1/(textAnimations.length + 1), 
			function() {
				element.html(element.html()+val.substring(0,1));
				animate();			
			
				if(val.length > 1){
					_animateText(val.substring(1,val.length),false,element);
				}
				else{
					lock = false;
					if(textAnimations.length > 0){
						animateText(textAnimations.shift());				
					}
				}
			}
		);
		}
		if(!firstLine){
			$("#messageArea").html($("#messageArea").html()+'\n');
		}
		firstLine = false;
		$("#messageArea").html($("#messageArea").html()+"<span style = 'color:"+color+";white-space: pre-wrap;' id = messageSpan"+lines+">"+"</span>");
		
		
	 	function blarg(){


			_animateText(text,true,$("#messageSpan"+lines));
		}
		blarg();
		++lines;
		

		function animate(){ //300
			if($("#messageArea")[0].scrollHeight != $("#messageArea").scrollTop() + $("#messageArea").height()){
				if( !$("#messageArea").is(':animated') ){
					$("#messageArea").stop().animate({scrollTop:$("#messageArea")[0].scrollHeight},1000,function(){animate();});
				}
			}
			/*else{
				$("#messageArea").stop();
			}*/
		}

	}


	function messageInfo(arg){
		animateText({val:arg,color:"yellow"});
	}
	function messageSpam(arg){
		animateText({val:arg,color:"white"});
	}
	function messageInstruction(arg){
		animateText({val:arg,color:"cyan"});
	}


/*
	for(var i = 0; i < 5; ++i){
	messageInfo("The fight phase has begun.");
	messageInstruction("Select an action for your unit [God of War] on square 4.");
	messageInfo("Your unit [God of war] on square 4 uses Flames of Battle.");
	messageSpam("Enemy unit [Apocalpyse Dragon] on square 1 gets hp -> 50.");
	messageSpam("Enemy unit [Apocalpyse Dragon] on square 3 gets hp -> 50.");
	messageSpam("Enemy unit [Haste Sorceress] on square 4 gets hp -> 50.");
	messageSpam("Enemy unit [Apocalpyse Dragon] on square 5 gets hp -> 50.");
	messageSpam("Enemy unit [Apocalpyse Dragon] on square 6 gets hp -> 50.");
	messageSpam("Enemy unit [Apocalpyse Dragon] on square 7 gets hp -> 50.");
	messageSpam("Enemy unit [Apocalpyse Dragon] on square 8 gets hp -> 50.");
	messageSpam("Enemy unit [Apocalpyse Dragon] on square 9 gets hp -> 50.");
	}
*/