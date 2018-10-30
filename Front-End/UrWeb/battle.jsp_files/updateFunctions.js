function numberMaker(boolean){
	if(boolean){
		return 0;
	}
	return 1;
}


var updateManager = function() {
	var publicScope = {
		addPayload:function(arg){
			console.log(arg);
			privateScope.payloads[arg.id] = arg.data;
			if(privateScope.unlocked === true){
				privateScope.executePayloads();
			}
		}
	};
	var privateScope = {
		lastPayload:-1, // 0 corresponds to the $.get request that starts the game.
		payloads:[],
		unlocked:true,
		executePayloads:function(){
			console.log("executing payloads");
			console.log(privateScope.lastPayload);
			if(privateScope.payloads[privateScope.lastPayload+1] !== undefined){
				privateScope.unlocked = false;
				++privateScope.lastPayload;
				privateScope.executePayload(privateScope.payloads[privateScope.lastPayload]);
			}
			else{
				privateScope.unlocked = true;	
			}
		},
		executePayload:function(data){
			console.log("executing payload : ");
			console.log(data);
			if(data.length > 0){
				var argument = data.shift();
				console.log("argument: ");
				console.log(argument);
				privateScope.updateFunctions[argument.type](
					function(){
						privateScope.executePayload(data);
					}, 
				argument);
			}
			else{
				privateScope.executePayloads();	
			}
		},
		updateFunctions:{
			
			
			/*
			CannotLeaveGameSearch:function(callback, arg){
				console.log("Cannot leave game search");
				console.log(arg);
				callback();
			},
			GotOutOfGameSearch:function(callback,arg){
				console.log("Got out of game search");
				console.log(arg);
				callback();
			},
			*/
			
			
			
			
			
			Attack:function(callback, arg) {
				callback();
			},
			ClockUpdate:function(callback, arg) {
				
				//console.log(arg);
				clock.playerTurn = arg.playerTurn;
				
				
				// This might not be the best place for this:
				
				phase.userTurn(arg.playerTurn);
				
				
				
				
				
				
				clock.playerFlagsAt = arg.playerFlagsAt;
				clock.enemyFlagsAt = arg.enemyFlagsAt;
				if(arg.playerTurn){
					//messageInstruction("It is your turn"); // could perhaps eventually get rid of this when I have more specific instructions.
					clock.playerTime = arg.playerTime;
					clock.enemyTime = arg.enemyFlagsAt - new Date().getTime();
				}
				else{
					clock.enemyTime = arg.enemyTime;
					clock.playerTime = arg.playerFlagsAt - new Date().getTime();
				}
				
				
				
//				clearInterval(clock.clockIntervalId);
//				clock.time = arg.playerTime;
//				clock.enemyTime = arg.enemyTime;
//				gameInterface.playerTurn = arg.playerTurn;
//					// I APPEAR TO HAVE PLAYER TURN IN BOTH GAME DATA AND GAME INTERFACE...
//				clock.clockIntervalId = setInterval(function() {
//					if(gameInterface.playerTurn) clock.updateClock();
//					else clock.updateEnemyClock();	  
//				}, 1000);
				
				
				callback();
			},
			EndSkillDataInput:function(callback, arg) {
				document.getElementById("skillComponentSelectionButton").style.display = "none";
				skillCurrently = false;
				callback();
			},
			StartSkillDataInput:function(callback, arg) {
				document.getElementById("skillComponentSelectionButton").style.display = "block";
				document.getElementById("cardSearchAreaOtherMore").style.display = "none";
				skillCurrently = true;
				updates.executeUpdates();
				callback();
			},
			Token:function(callback, arg) {
				console.log("calling token");
				googleChannel.token = arg.token.trim();
				sessionStorage.token = googleChannel.token;
				googleChannel.channel = new goog.appengine.Channel(googleChannel.token);
				var socket = googleChannel.channel.open();
				socket.onmessage = googleChannel.onMessage;
				socket.onerror = googleChannel.onError;
				socket.onclose = googleChannel.onClose;
				socket.onopen = function(){
					console.log("channel opened");
					
					console.log("initiating get call");
					$.get("tutorial",{"channelKey":sessionStorage.userId,"channelToken":sessionStorage.token} ,function(data){	
						console.log(data);
						var msg = {};
						msg.data = data;
						//console.log(msg);
						googleChannel.onMessage(msg);
								
					});
					// I should only be doing this if I opened the channel from information already present from the arena, so really
					// I should take Token out of update functions since it is never triggored from the server and could be disasterous if it was.
					
					
					
				}; /*googleChannel.onOpened;*/
				
				callback();
			},
			SkillLocationInformer:function(callback,arg){
				for(var i = 0; i < 9; ++i){
					$("#player"+i).removeClass("blink");
				}
				for(var i = 0; i < 9; ++i){
					$("#enemy"+i).removeClass("blink");
				}
				if(arg.position > -1){
					if(arg.player){
						$("#player"+arg.position).addClass("blink");
					}
					else{
						$("#enemy"+arg.position).addClass("blink");
					}
				}
				callback();
			},
			TriggorSkillField:function(callback, arg){
				// unimplemented
				//console.log("TRIGGORED SKILL ON FIELD!!!!!!");
				if(arg.player){
					gameInterface.animatePlayerSkill(callback, arg.target, arg.name)
				}
				else{
					gameInterface.animateEnemySkill(callback, arg.target, arg.name)
				}
				//console.log(arg);
			},
			TriggorSkillSet:function(callback,arg){
				// unimplemented
				callback();
			},
			TriggorSkillSoul:function(callback,arg){
				// unimplemented
				callback();
			},
			OpponentInformation:function(callback, arg) {
				//$(document.getElementById("oname")).html(arg.handle);
				//$(document.getElementById("orating")).html(arg.rating);
				console.log("playing against : " + arg.handle);
				
				$("#opponentNameSpan").text(arg.handle).html();
				$("#opponentRatingSpan").text(arg.rating).html();
				
			//	console.log("playing against : " + ${fn:escapeXml(arg.handle)});
			callback();	
			},
			PlayerRating:function(callback, arg){ // since I am storing the rating in session storage as well, this is overkill.
				$(document.getElementById("playerRatingSpan")).html(arg.rating);
				callback();
			},
			SetCard:function(callback, arg) {
				gameData.setData[numberMaker(arg.player)] = gameData.handData[numberMaker(arg.player)][arg.card];
				gameData.removeFromHand(arg.card,numberMaker(arg.player));
				gameInterface.setCard(numberMaker(arg.player));
				clock.playerTurn = arg.player;//playerToBool[arg.player];
				gameInterface.displayField(numberMaker(arg.player));
				callback();
			},
			//EnemySetThoughts:function(arg) { gameData.enemyThoughtCount = arg.thoughts; gameInterface.updateEnemyThoughts(); },
			SetKnowledge:function(callback, arg) {
				//console.error(arg);
				gameData.setKnowledge(numberMaker(arg.player), arg.knowledge);
				callback();
			},
			Skip:function(callback, arg) {
				if(numberMaker(arg.player) === 0) {
					clock.playerTurn = false;
				}
				else {
					clock.playerTurn = true;
				}
				callback();	
			},
			//SetThoughts:function(arg) { gameData.tempThoughtCount = arg.thoughts; gameData.thoughtCount = arg.thoughts; gameInterface.updateThoughts(); },
			SetThoughts:function(callback, arg){
				//console.error(arg);
				gameData.setThoughts(numberMaker(arg.player),arg.thoughts);
				callback();
			},
			GameStart:function(callback, arg) {
				console.log("Game Start");
				
				
				messageSpam("Welcome to Elemental Magicks. Your game has begun!");
				messageSpam("The game is composed of two rounds, with each player acting first in one.");
				messageSpam("In each round your goal to be the first to reduce your opponent to 0 soul points.");
				messageSpam("The players alternate losing one soul point every five turns, but can also lose them as a result of combat and control of the field.");
				messageSpam("Each round begins with the players adding cards to their hand from the totality of possible cards in the game.");
				messageSpam("You may draw as many copies of each card as you wish.");
				messageSpam("After you have each drawn 25 cards into your hand and 5 cards into your soul positions, the battle starts.");
				messageSpam("For a detailed description of the rules see the official rules page.");

				
				
				userInput.requestCards(); gameInterface.displayCardList();
				gameInterface.displayEnemyHand(); //gameInterface.enemyHandSelected = true;
				gameData.gameStarted = true;
				gameData.goFirst = arg.goFirst;
				clock.playerTurn = arg.goFirst;
				
			
				
				if(gameInterface.gameLoaded){
					$("#loadingScreen").fadeOut();
				}

/*
				if(gameData.goFirst){
					document.getElementById("turnNumberDiv").title =
						"You win agility tiebreaks on even numbered turns and lose them on odd numbered turns. On the fifth turn, and every ten turns after, you lose one soul point at the beginning of the removal phase. Your opponent also loses one soul point at the beginning of the removal phase every ten turns, beginning with the tenth turn.";
				}
				else{
					document.getElementById("turnNumberDiv").title =
						"You win agility tiebreaks on odd numbered turns and lose them on even numbered turns. On the tenth turn, and every ten turns after, you lose one soul point at the beginning of the removal phase. Your opponent also loses one soul point at the beginning of the removal phase every ten turns, beginning with the fifth turn.";

				}
				
	*/			
				
//				clearInterval(clock.clockIntervalId);
//				clock.time = arg.playerTime;
//				clock.enemyTime = arg.enemyTime;
//				gameInterface.playerTurn = arg.playerTurn;
//					// I APPEAR TO HAVE PLAYER TURN IN BOTH GAME DATA AND GAME INTERFACE...
				//clock.clockIntervalId =
				setInterval(function() {
					if(clock.playerTurn) clock.updateClock();
					else clock.updateEnemyClock();	  
				}, 500);
				callback();
			},
			
			
			// CURRENTLY JUST RESETTING MODEL FOR SOUL SELECTION...
			PlayerCard:function(callback, arg) {
				//document.getElementById("overlaystuff").style.display = "block";
				
				$("#playerSoulContainer"+phase.selectedPositions[6][0]).addClass("unselected").removeClass("selected");
				$("#playerSoulFace"+phase.selectedPositions[6][0]).addClass("unselected").removeClass("selected");
				$("#playerSoulBack"+phase.selectedPositions[6][0]).addClass("unselected").removeClass("selected");
				
				phase.selectedPositions[6][0] = -1;
				
				if(arg.player){
					
			//		document.getElementById("playerCardsDrawn").style.display = "block";
					gameData.cardsDrawn[0] += 1;		
			//		document.getElementById("playerCardsDrawn").style.opacity = 1;
			//		if(gameData.cardsDrawn[0] === 1){
			//			document.getElementById("playerCardsDrawn").innerHTML = gameData.cardsDrawn[0] + " Card Drawn";
			//		}
			//		else{
			//			document.getElementById("playerCardsDrawn").innerHTML = gameData.cardsDrawn[0] + " Cards Drawn";
			//		}
			//		fx(document.getElementById("playerCardsDrawn"));
					//$(document.getElementById('playerCardsDrawn')).html(gameData.cardsDrawn[0]);
					if(arg.hand) {
						messageInfo("You have drawn a card [" + arg.data.name+"] into your hand.");
						gameData.handData[0][arg.index] = new CardData();
						dealwithdeck(gameData.handData[0][arg.index],arg,false);
						phase.selectedPositions[6] = []; // deal with this
						$("#handImage"+arg.index).attr('src',gameData.handData[0][arg.index].imagehtml);
						document.getElementById("hand" + arg.index).style.visibility = "visible";
						// NOT DEALING WITH SOUL YET.
						
					}
					else {
						messageInfo("You have drawn a card [" + arg.data.name+"] into your soul at position " + (arg.index+1) + ".");
						gameData.soulData[0][arg.index] = new CardData();
						dealwithdeck(gameData.soulData[0][arg.index],arg,false);
					}
				}
				else{
			//		document.getElementById("enemyCardsDrawn").style.display = "block";
					gameData.cardsDrawn[1] += 1;
			//		document.getElementById("enemyCardsDrawn").style.opacity = 1;
			//		if(gameData.cardsDrawn[1] === 1){
			//			document.getElementById("enemyCardsDrawn").innerHTML = gameData.cardsDrawn[1] + " Card Drawn";
			//		}
			//		else{
			//			document.getElementById("enemyCardsDrawn").innerHTML = gameData.cardsDrawn[1] + " Cards Drawn";
			//		}
			//		fx(document.getElementById("enemyCardsDrawn"));
					//$(document.getElementById('enemyCardsDrawn')).html(gameData.cardsDrawn[1]);
					if(arg.hand) {
						messageInfo("Your opponent has drawn a card [" + arg.data.name+"] into their hand.");
						gameData.handData[1][arg.index] = new CardData();
						dealwithdeck(gameData.handData[1][arg.index],arg,true);
						phase.selectedPositions[7] = [];
						$("#enemyhandImage"+arg.index).attr('src',gameData.handData[1][arg.index].imagehtml);
						document.getElementById("enemyhand" + arg.index).style.visibility = "visible";
						// NOT DEALING WITH SOUL YET.
					}
					else {
						messageInfo("Your opponent has drawn a card [" + arg.data.name+"] into their soul at position " + (arg.index+1) + ".");
						gameData.soulData[1][arg.index] = new CardData();
						dealwithdeck(gameData.soulData[1][arg.index],arg,true);
					}
					gameInterface.displayEnemyHand(); // THIS WORKS? NEED TO RECALCULATE ENEMY HAND DISPLAY.
				}
				callback();
			},
			UserInstruction:function(callback, arg) { 
				//gameInterface.instruct(arg.instruction); gameInterface.print(arg.instruction); },
				messageInstruction(arg.instruction);
				callback();
			},
			
			// I am generating multiple change phase objects in the server currently, so this will only run once anyway.
			// This loop can also cause my client to crash if bugged.
			ChangePhase:function(callback, arg) {
				while(phase.getName() !== arg.phase) {
					console.log("changing phase from: " + phase.getName());
					phase.gotoNextPhase();
				}
				callback();
			}, 
			
			// currently the deploy card button isn't removed in some situations when both players set a card.
			DeployCard:function(callback, arg) {
				if(gameData.setData[numberMaker(arg.player)].name != "no card") {
					gameData.fieldData[numberMaker(arg.player)][arg.location] = gameData.setData[numberMaker(arg.player)];
					gameData.setData[numberMaker(arg.player)] = new NoCardData();
					gameInterface.displayFieldCard(arg.location,numberMaker(arg.player));
					gameInterface.displaySet(numberMaker(arg.player));
					
					if(arg.player) {
						phase.registerClick(0, arg.location, "player",false);
					}
				}
				callback();
			},
			
			////AT THIS POINT, RATHER THAN KEEPING TRACK OF THE UNIT CURRENTLY SELECTED BY UNIT TURN, I SHOULD DO THIS BUT WITH A DIFFERENT TYPE
			////OF SELECTION MECHANISM (SOME ANIMATION PERHAPS)
			UnitTurn:function(callback, arg) {
				gameInterface.unitTurn(arg.position,numberMaker(arg.player));
				callback();
			},
				// I could remove the action list at this point, but for now do nothing.
			Rest:function(callback, arg) { 
				callback();
			}, // enemy rest? Is this even being used??
			Move:function(callback, arg){
				gameData.fieldData[numberMaker(arg.player)][arg.end] = gameData.fieldData[numberMaker(arg.player)][arg.start];
				gameData.fieldData[numberMaker(arg.player)][arg.start] = new NoCardData();
				gameInterface.displayFieldCard(arg.end,numberMaker(arg.player));
				gameInterface.displayFieldCard(arg.start,numberMaker(arg.player));
				callback();
			},	
			ModifyStats:function(callback, arg) {
				var data = jQuery.parseJSON(arg.data),
				unit = gameData.fieldData[numberMaker(arg.player)][arg.target];
				
				//player8Engaged
				
				console.log("logging modify stats data");
				console.log(data);
				

				var dataArray = [];
				for(var stat in data) {
					dataArray.push([stat,data[stat]]);
				}
				function animateStatChange(){
					if(dataArray.length === 0){
						return callback();
					}
					var statAnimationCallback = function(){
						animateStatChange();
					}
					var currentStatChange = dataArray.shift();
					var stat = currentStatChange[0];
					unit[stat] = currentStatChange[1];
					if(stat === "engaged"){
						if(arg.player){
							if(unit[stat] == 0){
								gameInterface.disengagePlayerUnit(callback, arg.target);
							}
							else{
								gameInterface.engagePlayerUnit(callback, arg.target);
							}
						}
						else{
							if(unit[stat] == 0){
								gameInterface.disengageEnemyUnit(callback, arg.target);
							}
							else{
								gameInterface.engageEnemyUnit(callback, arg.target);
							}
						}
						return;
					}
					else{
						var value = unit[stat];
						if(stat === "currentAttack"){
							stat = "Attack"
						}
						else if(stat === "modifiedAttack"){
						//	return callback();
						}
						else if(stat === "currentAgility"){
							stat = "Agility"
						}
						else if(stat === "modifiedAgility"){
						//	return callback();
						}
						else if(stat === "currentDefense"){
							stat = "Defense";
						}
						else if(stat === "modifiedDefense"){
						//	return callback();
						}
						else if(stat === "currentRange"){
							stat = "Range";
						}
						else if(stat === "modifiedRange"){
						//	return callback();
						}
						else if(stat === "level"){ // DEPRECATED
							stat = "Level";
						}
						else if(stat === "modifiedLevel"){
						//	return callback();
						}
						else if(stat === "currentHP"){
							stat = "Hp";
						}
						else if(stat === "currentMana"){
							stat = "Mana";
						}
						else if(stat === "currentMaxHP"){
							stat = "Max Hp";
						}
						else if(stat === "currentMaxMana"){
							stat = "Max Mana";
						}
						else if(stat === "modifiedMaxHP"){
						//	return callback();
						}
						
						
						
						
						
						
						
						if(arg.player){
							gameInterface.animatePlayerStatChange(callback, arg.target, stat, value);
						}
						else{
							gameInterface.animateEnemyStatChange(callback, arg.target, stat, value);
						}
					}
				}
				animateStatChange();
			},
			Die:function(callback, arg) {
				gameData.fieldData[numberMaker(arg.player)][arg.position].dead = true;
				gameInterface.killField(arg.position,numberMaker(arg.player));
				callback();
			},
			ReviveCards:function(callback, arg) {
				for(var i = arg.revivedCards.length - 1; i > -1; --i){
					if(arg.revivedCards[i] == true) { 
						gameData.reviveCard(i,numberMaker(arg.player));
					}
				}
				callback();
			},
			SwitchSetAndGraveyard:function(callback, arg){
				if(gameData.setData[numberMaker(arg.player)].name === "no card"){
					gameData.setData[numberMaker(arg.player)] = gameData.graveyardData[numberMaker(arg.player)][arg.graveyardIndex];
					gameData.removeFromGraveyard(arg.graveyardIndex,numberMaker(arg.player));
					gameInterface.displaySet(numberMaker(arg.player));
				}
				else{
					gameData.setData[numberMaker(arg.player)] = swapArray(gameData.graveyardData[numberMaker(arg.player)],gameData.setData[numberMaker(arg.player)],arg.graveyardIndex);
					gameInterface.displayPlayerSet();
				}
				gameInterface.updateGraveyard(numberMaker(arg.player));
				callback();
			},
			//sendFromS
			// OTHERS ABOVE ETC MAY BE BROKEN 
			SendFromHandToBoard:function(callback, arg) { // MAYBE BROKEN
				gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex] = gameData.handData[numberMaker(arg.player)][arg.handIndex];
				gameData.removeFromHand(arg.handIndex,numberMaker(arg.player));
				gameInterface.displayFieldCard(arg.fieldIndex,numberMaker(arg.player));
				gameInterface.displayField(numberMaker(arg.player));
				gameInterface.updateHand(numberMaker(arg.player));
				callback();
			},
			SendFromGraveyardToGraveyard:function(callback, arg) { // BROKEN
				//gameData.addToGraveyard(gameData.handData[0][arg.index]);
				//gameData.removeFromPlayerHand(arg.index);
				
				var card = gameData.graveyardData[numberMaker(arg.player)][fromIndex];
				gameData.removeFromGraveyard(arg.fromIndex,numberMaker(arg.player));
				gameData.graveyardData[numberMaker(arg.player)].splice(arg.toIndex,0,card);
				gameInterface.updateGraveyard(numberMaker(arg.player));
				callback();
			},
			SendFromHandToGraveyard:function(callback, arg) {
				//gameData.addToGraveyard(gameData.handData[0][arg.fromIndex], arg.toIndex, arg.player);
				gameData.graveyardData[numberMaker(arg.player)].splice(arg.toIndex,0,gameData.handData[numberMaker(arg.player)][arg.fromIndex]); // BROKEN?
				gameData.removeFromHand(arg.fromIndex,numberMaker(arg.player));
				gameInterface.updateGraveyard(numberMaker(arg.player));
				gameInterface.updateHand(numberMaker(arg.player));
				callback();
			},
			SendFromBoardToGraveyard:function(callback, arg) {
				gameData.reviveCard(arg.fieldIndex,numberMaker(arg.player)); // added this line.
				gameData.graveyardData[numberMaker(arg.player)].splice(arg.graveyardIndex, 0, gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex]);
				gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex] = new NoCardData();
				gameInterface.displayFieldCard(arg.fieldIndex,numberMaker(arg.player));
				gameInterface.updateGraveyard(numberMaker(arg.player));
				callback();
			},
			SendFromBoardToHand:function(callback, arg) {
				gameData.handData[numberMaker(arg.player)].splice(arg.handIndex, 0, gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex]);
				gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex] = new NoCardData();
				gameInterface.displayFieldCard(arg.fieldIndex,numberMaker(arg.player));
				gameInterface.updateHand(numberMaker(arg.player));
				callback();
			},
			SendFromGraveyardToHand:function(callback, arg) {
				
				// THERE IS NOTHING HERE TO LET ME ADDRESS THE TOINDEX..........
				
				
				
				
				
				gameData.addToHand(gameData.graveyardData[numberMaker(arg.player)][arg.fromIndex],numberMaker(arg.player));
				gameData.removeFromGraveyard(arg.fromIndex,numberMaker(arg.player));
				gameInterface.updateGraveyard(numberMaker(arg.player));
				gameInterface.updateHand(numberMaker(arg.player));
				callback();
			},
			SwitchFieldAndGraveyard:function(callback,arg){
				var p = numberMaker(arg.player);
				if(gameData.fieldData[p][arg.fieldIndex].name === "no card"){
					console.log("blarg blarg blarg");
					gameData.fieldData[p][arg.fieldIndex] = gameData.graveyardData[p][arg.graveyardIndex];
					gameData.removeFromGraveyard(arg.graveyardIndex,p);
					gameInterface.updateGraveyard(p);
					gameInterface.displayFieldCard(arg.fieldIndex,p);
					callback();
				}
				else{
					console.log("grrrrrrrrrrrrrrrrrrrrr");
					var temp777 = gameData.fieldData[p][arg.fieldIndex];
					gameData.fieldData[p][arg.fieldIndex] = gameData.graveyardData[p][arg.graveyardIndex];
					gameData.graveyardData[p].splice(arg.graveyardIndex, 1, temp777);
					gameData.removeFromHand(arg.graveyardIndex,p);
					gameInterface.updateGraveyard(p);
					gameInterface.displayFieldCard(arg.fieldIndex,p);
					callback();
				}
			},
			
			/// I HAVEN'T FULLY VETTED ALL OF THESE SWITCHING AND SENDING FUNCTIONS, OR EVEN ORGANIZED THEM!
			
			TakeLifeDamage:function(callback, arg) {
				gameData.takeLifeDamage(arg.amount,numberMaker(arg.player),callback);
			},
			ServePartialCardList:function(callback, arg) {
				console.log(arg);
				cards = [];
				cardList.cardIds = [];
				for(var i = 0; i < 25; ++i){
					document.getElementById("cardList" + i).style.visibility = "hidden";
				}
				for(var i = 0; i < arg.data.length; ++i) {
					cards.push({});
					var source = arg.data[i];
					for(var j = 0, k = ["hp","attack","defense","agility","range","level","mana"]; j < k.length; ++j) {
						cards[i][k[j]] = source.stats[k[j]];
					}
					for(var j = 0, k = ["actionSkills","actionSkillsName","actionSkillsCost","autoSkills","autoSkillsName","autoSkillsCost",
					                    "openSkills","openSkillsName","openSkillsCost","counterSkills","counterSkillsName","counterSkillsCost",
					                    "startSkills","startSkillsName","startSkillsCost","endSkills","endSkillsName","endSkillsCost",
					                    "soulSkills","soulSkillsName","soulSkillsCost","closeSkills","closeSkillsName","closeSkillsCost"]; j < k.length; ++j) {
						cards[i][k[j]] = source[k[j]];
					}
					cards[i].name = source.name;
					cards[i].imagehtml = '/images/' + source.imagehtml + '.jpg';
					cards[i].displayhtml = '/images/' + source.imagehtml + 'Display.jpg';
					cards[i].LP = source.stats.lp;
					cards[i].school = source.stats.schoolsOfThought;
					cards[i].id = source.id;
					/////cardList.cardIds[i] = arg.data[i].id;
					
					$("#cardListImage"+i).attr('src',cards[i].imagehtml);
					document.getElementById("cardList" + i).style.visibility = "visible";
				}
				if(arg.noPreviousPage){
					document.getElementById("cardListLeft").style.visibility = 'hidden';
				}
				else{
					document.getElementById("cardListLeft").style.visibility = 'visible';
				}
				if(arg.noNextPage){
					document.getElementById("cardListRight").style.visibility = 'hidden';
				}
				else{
					document.getElementById("cardListRight").style.visibility = 'visible';
				}
				gameInterface.displayCardList();
				callback();
			},
			GameWon:function(callback, arg){
				document.getElementById("playerRatingSpan").innerHTML +="->"+arg.playerRating;
				document.getElementById("opponentRatingSpan").innerHTML +="->"+arg.opponentRating;
				callback();
			},
			RoundWon:function(callback, arg) {
				function beginNextRound(){
					console.log("beginning next round");
					clock.reset();
					phase.reset();
					gameData.reset();
					
					for(var i = 0; i < 9; ++i){
						$("#player"+i).removeClass("blink");
					}
					for(var i = 0; i < 9; ++i){
						$("#enemy"+i).removeClass("blink");
					}
					
					
					document.getElementById("enemyIcons").style.visibility = "hidden";
					
					
					document.getElementById("cardSearchButton").style.visibility = "visible";
					document.getElementById("enemyResources").style.display = "none";
					/////$("#playerThoughtFigure").removeClass("ball-thoughts-text").removeClass("ball-thoughts-selected-text").removeClass("ball-thoughts-selected").addClass("ball-thoughts");
					//document.getElementById("playerThoughtSpan").innerHTML = 5;
					document.getElementById("cardSearchAreaOther").style.display = "none";
					document.getElementById("cardSearchArea").style.display = "block";
					/*$("#cardSearchArea").html('<div style = "text-align:center; color:lightgrey;" class = input >Card Search</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
								'<div class="container1ffff">'+
									'<div class="col1ffff">'+
										'<div class = "input2" style = "color:white; visibility:hidden;">...</div>'+
									'</div>'+
									'<div class="col2ffff">'+
										'<div style = "color:grey; text-align:center" class = "input2">min</div>'+
									'</div>'+
									'<div class="col3ffff">'+
										'<div style = "color:grey; text-align:center" class = "input2">max</div>'+
									'</div>'+
								'</div>'+
							'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
								'<div class="container2ffff">'+
									'<div class="container1ffff">'+
										'<div class="col1ffff">'+
											'<div class = "input2" style = "color:white">level</div>'+
										'</div>'+
										'<div class="col2ffff">'+
											'<input class = "input levelInput" id = "minLevel" >'+
										'</div>'+
										'<div class="col3ffff">'+
							    			'<input class = "input levelInput" id = "maxLevel" >'+
										'</div>'+
									'</div>'+
								'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
								'<div class="container1ffff">'+
									'<div class="col1ffff">'+
										'<div class = "input2" style = "color:white">hp</div>'+
									'</div>'+
									'<div class="col2ffff">'+
										'<input class = "input hpInput" id = "minHp" >'+
									'</div>'+
									'<div class="col3ffff">'+
										'<input class = "input hpInput" id = "maxHp" >'+
									'</div>'+
								'</div>'+
							'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
							'<div class="container1ffff">'+
								'<div class="col1ffff">'+
									'<div class = "input2" style = "color:white">mana</div>'+
								'</div>'+
								'<div class="col2ffff">'+
									'<input class = "input hpInput" id = "minMana" >'+
								'</div>'+
								'<div class="col3ffff">'+
									'<input class = "input hpInput" id = "maxMana" >'+
								'</div>'+
							'</div>'+
							'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
							'<div class="container1ffff">'+
								'<div class="col1ffff">'+
									'<div class = "input2" style = "color:white">attack</div>'+
								'</div>'+
								'<div class="col2ffff">'+
									'<input class = "input levelInput" id = "minAttack" >'+
								'</div>'+
								'<div class="col3ffff">'+
									'<input class = "input levelInput" id = "maxAttack" >'+
								'</div>'+
							'</div>'+
							'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
							'<div class="container1ffff">'+
								'<div class="col1ffff">'+
									'<div class = "input2" style = "color:white">defense</div>'+
								'</div>'+
								'<div class="col2ffff">'+
									'<input class = "input levelInput" id = "minDefense" >'+
								'</div>'+
								'<div class="col3ffff">'+
									'<input class = "input levelInput" id = "maxDefense" >'+
								'</div>'+
							'</div>'+
							'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
							'<div class="container1ffff">'+
								'<div class="col1ffff">'+
									'<div class = "input2" style = "color:white">agility</div>'+
								'</div>'+
								'<div class="col2ffff">'+
									'<input class = "input levelInput" id = "minAgility" >'+
								'</div>'+
								'<div class="col3ffff">'+
									'<input class = "input levelInput" id = "maxAgility" >'+
								'</div>'+
							'</div>'+
							'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
							'<div class="container1ffff">'+
								'<div class="col1ffff">'+
									'<div class = "input2" style = "color:white">range</div>'+
								'</div>'+
								'<div class="col2ffff">'+
									'<input class = "input levelInput" id = "minRange" >'+
								'</div>'+
								'<div class="col3ffff">'+
									'<input class = "input levelInput" id = "maxRange" >'+
								'</div>'+
							'</div>'+
							'</div>'+
							'</div>'+
							'<div class="container3ffff">'+
							'<div class="container2ffff">'+
							'<div class="container1ffff">'+
								'<div class="col1ffff">'+
									'<div class = "input2" style = "color:white">sp</div>'+
								'</div>'+
								'<div class="col2ffff">'+
									'<input class = "input levelInput" id = "minSp" >'+
								'</div>'+
								'<div class="col3ffff">'+
									'<input class = "input levelInput" id = "maxSp" >'+
								'</div>'+
							'</div>'+
							'</div>'+
							'</div>');
					*/
				}
				if(arg.player === true){
					gameData.wins += 1; alert("You won this round");
					if(gameData.wins == 1 && gameData.losses == 1) {
						alert("The game was drawn");
					}
					else if(gameData.wins == 2) {
						alert("You won the game");
					}
					else {
						//document.getElementById("pwins").innerHTML = "wins: 1";
						alert("beginning next round");
						beginNextRound();
					}
				}
				else{
					gameData.losses += 1; alert("You lost this round");
					if(gameData.wins == 1 && gameData.losses == 1) {
						alert("The game was drawn");
					}
					else if(gameData.losses == 2){
						alert("You lost the game");
					}
					else {
						//document.getElementById("owins").innerHTML = "wins: 1";
						alert("beginning next round"); 
						beginNextRound();
					}
				}
				callback();
			},
			RowTargetUpdate:function(callback, arg) {
				gameInterface.moveNextTarget(arg.row, (arg.column+1)%3, numberMaker(arg.player));
				callback();
			},
			SwitchPositions:function(callback, arg) {
				// MOVING (PERHAPS NOT MOVING HERE) DOES NOT ALWAYS RESTORE ONCLICK FUNCTIONS. (move and then target with any action skill the same engagement phase).
				var temp = gameData.fieldData[numberMaker(arg.player)][arg.a];
				gameData.fieldData[numberMaker(arg.player)][arg.a] = gameData.fieldData[numberMaker(arg.player)][arg.b];
				gameData.fieldData[numberMaker(arg.player)][arg.b] = temp;
				gameInterface.displayFieldCard(arg.a,numberMaker(arg.player));
				gameInterface.displayFieldCard(arg.b,numberMaker(arg.player));
				callback();
			}
		// CAN PEOPLE MOVE TO THE SAME LOCATION? DO I HAVE SAFEGUARDS TO CONTINUE WITHOUT PROMPTING IF NO MOVE IS AVAILABLE?
		}
	};
	return publicScope;
}();























































// have this here for now to deal with out of game updates and stuff. Will soon refactor... just want to get stuff working first.

var updateFunctions =
{
		
		CannotLeaveGameSearch:function(arg){
			console.log("Cannot leave game search");
			console.log(arg);
			
			/////callback();
		},
		GotOutOfGameSearch:function(arg){
			console.log("Got out of game search");
			console.log(arg);
			window.location = "/Arena";
			////callback();
		},
		
		
	Attack:function(arg) {},
	ClockUpdate:function(arg) {
		
		//console.log(arg);
		clock.playerTurn = arg.playerTurn;
		clock.playerFlagsAt = arg.playerFlagsAt;
		clock.enemyFlagsAt = arg.enemyFlagsAt;
		if(arg.playerTurn){
			//messageInstruction("It is your turn"); // could perhaps eventually get rid of this when I have more specific instructions.
			clock.playerTime = arg.playerTime;
			clock.enemyTime = arg.enemyFlagsAt - new Date().getTime();
		}
		else{
			clock.enemyTime = arg.enemyTime;
			clock.playerTime = arg.playerFlagsAt - new Date().getTime();
		}
		
		
		
//		clearInterval(clock.clockIntervalId);
//		clock.time = arg.playerTime;
//		clock.enemyTime = arg.enemyTime;
//		gameInterface.playerTurn = arg.playerTurn;
//			// I APPEAR TO HAVE PLAYER TURN IN BOTH GAME DATA AND GAME INTERFACE...
//		clock.clockIntervalId = setInterval(function() {
//			if(gameInterface.playerTurn) clock.updateClock();
//			else clock.updateEnemyClock();	  
//		}, 1000);
	},
	EndSkillDataInput:function(arg) {
		document.getElementById("cardSearchAreaOther").innerHTML = "";
	},
	StartSkillDataInput:function(callback, arg) {
		document.getElementById("cardSearchAreaOther").innerHTML = "<button class = 'myButton' onClick = 'gameInterface.confirmSkillComponentSelection();'>Ok</button>";
		document.getElementById("cardSearchAreaOtherMore").style.display = "none";
		updates.executeUpdates();
	},
	Token:function(arg) {
		console.log("calling token");
		googleChannel.token = arg.token.trim();
		sessionStorage.token = googleChannel.token;
		googleChannel.channel = new goog.appengine.Channel(googleChannel.token);
		var socket = googleChannel.channel.open();
		socket.onmessage = googleChannel.onMessage;
		socket.onerror = googleChannel.onError;
		socket.onclose = googleChannel.onClose;
		socket.onopen = function(){
			console.log("channel opened");
			
			console.log("initiating get call");
			$.get("tutorial",{"channelKey":sessionStorage.userId,"channelToken":sessionStorage.token} ,function(data){	
				console.log(data);
				var msg = {};
				msg.data = data;
				//console.log(msg);
				googleChannel.onMessage(msg);
						
			});
			// I should only be doing this if I opened the channel from information already present from the arena, so really
			// I should take Token out of update functions since it is never triggored from the server and could be disasterous if it was.
			
			
			
		}; /*googleChannel.onOpened;*/
		
		
	},
	OpponentInformation:function(arg) {
		//$(document.getElementById("oname")).html(arg.handle);
		//$(document.getElementById("orating")).html(arg.rating);
		console.log("playing against : " + arg.handle);
		
		$("#opponentNameSpan").text(arg.handle).html();
		$("#opponentRatingSpan").text(arg.rating).html();
		
	//	console.log("playing against : " + ${fn:escapeXml(arg.handle)});
		
	},
	SetCard:function(arg) {
		gameData.setData[numberMaker(arg.player)] = gameData.handData[numberMaker(arg.player)][arg.card];
		gameData.removeFromHand(arg.card,numberMaker(arg.player));
		gameInterface.setCard(numberMaker(arg.player));
		clock.playerTurn = arg.player;//playerToBool[arg.player];
		gameInterface.displayField(numberMaker(arg.player));
	},
	//EnemySetThoughts:function(arg) { gameData.enemyThoughtCount = arg.thoughts; gameInterface.updateEnemyThoughts(); },
	SetKnowledge:function(arg) {
		//console.error(arg);
		gameData.setKnowledge(numberMaker(arg.player), arg.knowledge);
	},
	Skip:function(callback, arg) {if(numberMaker(arg.player) === 0) clock.playerTurn = false; else clock.playerTurn = true;},
	//SetThoughts:function(arg) { gameData.tempThoughtCount = arg.thoughts; gameData.thoughtCount = arg.thoughts; gameInterface.updateThoughts(); },
	SetThoughts:function(arg){
		//console.error(arg);
		gameData.setThoughts(numberMaker(arg.player),arg.thoughts);
	},
	GameStart:function(arg) {
		console.log("Game Start");
		
		// this function not called in this update function object.
		
		
		userInput.requestCards(); gameInterface.displayCardList();
		gameInterface.displayEnemyHand(); //gameInterface.enemyHandSelected = true;
		gameData.gameStarted = true;
		gameData.goFirst = arg.goFirst;
		clock.playerTurn = arg.goFirst;
		
		
//		clearInterval(clock.clockIntervalId);
//		clock.time = arg.playerTime;
//		clock.enemyTime = arg.enemyTime;
//		gameInterface.playerTurn = arg.playerTurn;
//			// I APPEAR TO HAVE PLAYER TURN IN BOTH GAME DATA AND GAME INTERFACE...
		//clock.clockIntervalId =
		setInterval(function() {
			if(clock.playerTurn) clock.updateClock();
			else clock.updateEnemyClock();	  
		}, 500);
		
	},
	
	
	// CURRENTLY JUST RESETTING MODEL FOR SOUL SELECTION...
	PlayerCard:function(arg) {
		//document.getElementById("overlaystuff").style.display = "block";
		
		$("#playerSoulContainer"+phase.selectedPositions[6][0]).addClass("unselected").removeClass("selected");
		$("#playerSoulFace"+phase.selectedPositions[6][0]).addClass("unselected").removeClass("selected");
		$("#playerSoulBack"+phase.selectedPositions[6][0]).addClass("unselected").removeClass("selected");
		
		phase.selectedPositions[6][0] = -1;
		
		if(arg.player){
			
	//		document.getElementById("playerCardsDrawn").style.display = "block";
			gameData.cardsDrawn[0] += 1;		
	//		document.getElementById("playerCardsDrawn").style.opacity = 1;
	//		if(gameData.cardsDrawn[0] === 1){
	//			document.getElementById("playerCardsDrawn").innerHTML = gameData.cardsDrawn[0] + " Card Drawn";
	//		}
	//		else{
	//			document.getElementById("playerCardsDrawn").innerHTML = gameData.cardsDrawn[0] + " Cards Drawn";
	//		}
	//		fx(document.getElementById("playerCardsDrawn"));
			//$(document.getElementById('playerCardsDrawn')).html(gameData.cardsDrawn[0]);
			if(arg.hand) {
				messageInfo("You have drawn a card [" + arg.data.name+"] into your hand.");
				gameData.handData[0][arg.index] = new CardData();
				dealwithdeck(gameData.handData[0][arg.index],arg,false);
				phase.selectedPositions[6] = []; // deal with this
				$("#handImage"+arg.index).attr('src',gameData.handData[0][arg.index].imagehtml);
				document.getElementById("hand" + arg.index).style.visibility = "visible";
				// NOT DEALING WITH SOUL YET.
				
			}
			else {
				messageInfo("You have drawn a card [" + arg.data.name+"] into your soul at position " + (arg.index+1) + ".");
				gameData.soulData[0][arg.index] = new CardData();
				dealwithdeck(gameData.soulData[0][arg.index],arg,false);
			}
		}
		else{
	//		document.getElementById("enemyCardsDrawn").style.display = "block";
			gameData.cardsDrawn[1] += 1;
	//		document.getElementById("enemyCardsDrawn").style.opacity = 1;
	//		if(gameData.cardsDrawn[1] === 1){
	//			document.getElementById("enemyCardsDrawn").innerHTML = gameData.cardsDrawn[1] + " Card Drawn";
	//		}
	//		else{
	//			document.getElementById("enemyCardsDrawn").innerHTML = gameData.cardsDrawn[1] + " Cards Drawn";
	//		}
	//		fx(document.getElementById("enemyCardsDrawn"));
			//$(document.getElementById('enemyCardsDrawn')).html(gameData.cardsDrawn[1]);
			if(arg.hand) {
				messageInfo("Your opponent has drawn a card [" + arg.data.name+"] into their hand.");
				gameData.handData[1][arg.index] = new CardData();
				dealwithdeck(gameData.handData[1][arg.index],arg,true);
				phase.selectedPositions[7] = [];
				$("#enemyhandImage"+arg.index).attr('src',gameData.handData[1][arg.index].imagehtml);
				document.getElementById("enemyhand" + arg.index).style.visibility = "visible";
				// NOT DEALING WITH SOUL YET.
			}
			else {
				messageInfo("Your opponent has drawn a card [" + arg.data.name+"] into their soul at position " + (arg.index+1) + ".");
				gameData.soulData[1][arg.index] = new CardData();
				dealwithdeck(gameData.soulData[1][arg.index],arg,true);
			}
			gameInterface.displayEnemyHand(); // THIS WORKS? NEED TO RECALCULATE ENEMY HAND DISPLAY.
		}
	},
	UserInstruction:function(arg) { 
		//gameInterface.instruct(arg.instruction); gameInterface.print(arg.instruction); },
		messageInstruction(arg.instruction);
	},
	
	// I am generating multiple change phase objects in the server currently, so this will only run once anyway.
	// This loop can also cause my client to crash if bugged.
	ChangePhase:function(arg) { while(phase.getName() !== arg.phase) { phase.gotoNextPhase(); } }, 
	
	// currently the deploy card button isn't removed in some situations when both players set a card.
	DeployCard:function(arg) {
		if(gameData.setData[numberMaker(arg.player)].name != "no card") {
			gameData.fieldData[numberMaker(arg.player)][arg.location] = gameData.setData[numberMaker(arg.player)];
			gameData.setData[numberMaker(arg.player)] = new NoCardData();
			gameInterface.displayFieldCard(arg.location,numberMaker(arg.player));
			gameInterface.displaySet(numberMaker(arg.player));
		}
	},
	
	////AT THIS POINT, RATHER THAN KEEPING TRACK OF THE UNIT CURRENTLY SELECTED BY UNIT TURN, I SHOULD DO THIS BUT WITH A DIFFERENT TYPE
	////OF SELECTION MECHANISM (SOME ANIMATION PERHAPS)
	UnitTurn:function(arg) { gameInterface.unitTurn(arg.position,numberMaker(arg.player)); },
		// I could remove the action list at this point, but for now do nothing.
	Rest:function(arg) { }, // enemy rest? Is this even being used??
	Move:function(arg){
		gameData.fieldData[numberMaker(arg.player)][arg.end] = gameData.fieldData[arg.player][arg.start];
		gameData.fieldData[numberMaker(arg.player)][arg.start] = new NoCardData();
		gameInterface.displayFieldCard(arg.end,numberMaker(arg.player));
		gameInterface.displayFieldCard(arg.start,numberMaker(arg.player));
	},	
	ModifyStats:function(arg) {
		var data = jQuery.parseJSON(arg.data),
		unit = gameData.fieldData[numberMaker(arg.player)][arg.target];
		for(var stat in data) { unit[stat] = data[stat]; }
	},
	Die:function(arg) { gameData.fieldData[numberMaker(arg.player)][arg.position].dead = true; gameInterface.killField(arg.position,numberMaker(arg.player)); },
	ReviveCards:function(arg) { for(var i = arg.revivedCards.length - 1; i > -1; --i) { if(arg.revivedCards[i] == true) gameData.reviveCard(i,numberMaker(arg.player)); } },
	SwitchSetAndGraveyard:function(arg){
		if(gameData.setData[numberMaker(arg.player)].name === "no card"){
			gameData.setData[numberMaker(arg.player)] = gameData.graveyardData[numberMaker(arg.player)][arg.graveyardIndex];
			gameData.removeFromGraveyard(arg.graveyardIndex,numberMaker(arg.player));
			gameInterface.displaySet(numberMaker(arg.player));
		}
		else{
			gameData.setData[numberMaker(arg.player)] = swapArray(gameData.graveyardData[numberMaker(arg.player)],gameData.setData[numberMaker(arg.player)],arg.graveyardIndex);
			gameInterface.displayPlayerSet();
		}
		gameInterface.updateGraveyard(numberMaker(arg.player));
	},
	//sendFromS
	// OTHERS ABOVE ETC MAY BE BROKEN 
	SendFromHandToBoard:function(arg) { // MAYBE BROKEN
		gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex] = gameData.handData[numberMaker(arg.player)][arg.handIndex];
		gameData.removeFromHand(arg.handIndex,numberMaker(arg.player));
		gameInterface.displayFieldCard(arg.fieldIndex,numberMaker(arg.player));
		gameInterface.displayField(numberMaker(arg.player));
		gameInterface.updateHand(numberMaker(arg.player));
	},
	SendFromGraveyardToGraveyard:function(arg) { // BROKEN
		//gameData.addToGraveyard(gameData.handData[0][arg.index]);
		//gameData.removeFromPlayerHand(arg.index);
		
		var card = gameData.graveyardData[numberMaker(arg.player)][fromIndex];
		gameData.removeFromGraveyard(arg.fromIndex,numberMaker(arg.player));
		gameData.graveyardData[numberMaker(arg.player)].splice(arg.toIndex,0,card);
		gameInterface.updateGraveyard(numberMaker(arg.player));
	},
	SendFromHandToGraveyard:function(arg) {
		//gameData.addToGraveyard(gameData.handData[0][arg.fromIndex], arg.toIndex, arg.player);
		gameData.graveyardData[numberMaker(arg.player)].splice(arg.toIndex,0,gameData.handData[numberMaker(arg.player)][arg.fromIndex]);
		gameData.removeFromHand(arg.fromIndex,numberMaker(arg.player));
		gameInterface.updateGraveyard(numberMaker(arg.player));
		gameInterface.updateHand(numberMaker(arg.player));
	},
	SendFromBoardToGraveyard:function(arg) {
		gameData.graveyardData[numberMaker(arg.player)].splice(arg.graveyardIndex, 0, gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex]);
		gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex] = new NoCardData();
		gameInterface.displayFieldCard(arg.fieldIndex,numberMaker(arg.player));
		gameInterface.updateGraveyard(numberMaker(arg.player));
	},
	SendFromBoardToHand:function(arg) {
		gameData.handData[numberMaker(arg.player)].splice(arg.handIndex, 0, gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex]);
		gameData.fieldData[numberMaker(arg.player)][arg.fieldIndex] = new NoCardData();
		gameInterface.displayFieldCard(arg.fieldIndex,numberMaker(arg.player));
		gameInterface.updateHand(numberMaker(arg.player));
	},
	SendFromGraveyardToHand:function(arg) {
		gameData.addToHand(gameData.graveyardData[arg.player][arg.index],numberMaker(arg.player));
		gameData.removeFromGraveyard(arg.index,numberMaker(arg.player));
		gameInterface.updateGraveyard(numberMaker(arg.player));
		gameInterface.updateHand(numberMaker(arg.player));	
	},
	TakeLifeDamage:function(arg) {
		gameData.takeLifeDamage(arg.amount,numberMaker(arg.player));
	},
	ServePartialCardList:function(arg) {
		console.log(arg);
		cards = [];
		cardList.cardIds = [];
		for(var i = 0; i < 25; ++i){
			document.getElementById("cardList" + i).style.visibility = "hidden";
		}
		for(var i = 0; i < arg.data.length; ++i) {
			cards.push({});
			var source = arg.data[i];
			for(var j = 0, k = ["hp","attack","defense","agility","range","level","mana"]; j < k.length; ++j) {
				cards[i][k[j]] = source.stats[k[j]];
			}
			for(var j = 0, k = ["actionSkills","actionSkillsName","actionSkillsCost","autoSkills","autoSkillsName","autoSkillsCost",
			                    "openSkills","openSkillsName","openSkillsCost","counterSkills","counterSkillsName","counterSkillsCost",
			                    "startSkills","startSkillsName","startSkillsCost","endSkills","endSkillsName","endSkillsCost",
			                    "soulSkills","soulSkillsName","soulSkillsCost","closeSkills","closeSkillsName","closeSkillsCost"]; j < k.length; ++j) {
				cards[i][k[j]] = source[k[j]];
			}
			cards[i].name = source.name;
			cards[i].imagehtml = '/images/' + source.imagehtml + '.jpg';
			cards[i].displayhtml = '/images/' + source.imagehtml + 'Display.jpg';
			cards[i].LP = source.stats.lp;
			cards[i].school = source.stats.schoolsOfThought;
			cards[i].id = source.id;
			/////cardList.cardIds[i] = arg.data[i].id;
			
			$("#cardListImage"+i).attr('src',cards[i].imagehtml);
			document.getElementById("cardList" + i).style.visibility = "visible";
		}
		if(arg.noPreviousPage){
			document.getElementById("cardListLeft").style.visibility = 'hidden';
		}
		else{
			document.getElementById("cardListLeft").style.visibility = 'visible';
		}
		if(arg.noNextPage){
			document.getElementById("cardListRight").style.visibility = 'hidden';
		}
		else{
			document.getElementById("cardListRight").style.visibility = 'visible';
		}
		gameInterface.displayCardList();
	},
	GameWon:function(arg){
		document.getElementById("playerRatingSpan").innerHTML +="->"+arg.playerRating;
		document.getElementById("opponentRatingSpan").innerHTML +="->"+arg.opponentRating;
	},
	RoundWon:function(arg) {
		function beginNextRound(){
			console.log("beginning next round");
			clock.reset();
			phase.reset();
			gameData.reset();
			
			
			document.getElementById("cardSearchButton").style.visibility = "visible";
			document.getElementById("enemyResources").style.display = "none";
			/////$("#playerThoughtFigure").removeClass("ball-thoughts-text").removeClass("ball-thoughts-selected-text").removeClass("ball-thoughts-selected").addClass("ball-thoughts");
			//document.getElementById("playerThoughtSpan").innerHTML = 5;
			
			document.getElementById("cardSearchAreaOther").style.display = "none";
			document.getElementById("cardSearchArea").style.display = "block";
			
			/*
			$("#cardSearchArea").html('<div style = "text-align:center; color:lightgrey;" class = input >Card Search</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
						'<div class="container1ffff">'+
							'<div class="col1ffff">'+
								'<div class = "input2" style = "color:white; visibility:hidden;">...</div>'+
							'</div>'+
							'<div class="col2ffff">'+
								'<div style = "color:grey; text-align:center" class = "input2">min</div>'+
							'</div>'+
							'<div class="col3ffff">'+
								'<div style = "color:grey; text-align:center" class = "input2">max</div>'+
							'</div>'+
						'</div>'+
					'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
						'<div class="container2ffff">'+
							'<div class="container1ffff">'+
								'<div class="col1ffff">'+
									'<div class = "input2" style = "color:white">level</div>'+
								'</div>'+
								'<div class="col2ffff">'+
									'<input class = "input levelInput" id = "minLevel" >'+
								'</div>'+
								'<div class="col3ffff">'+
					    			'<input class = "input levelInput" id = "maxLevel" >'+
								'</div>'+
							'</div>'+
						'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
						'<div class="container1ffff">'+
							'<div class="col1ffff">'+
								'<div class = "input2" style = "color:white">hp</div>'+
							'</div>'+
							'<div class="col2ffff">'+
								'<input class = "input hpInput" id = "minHp" >'+
							'</div>'+
							'<div class="col3ffff">'+
								'<input class = "input hpInput" id = "maxHp" >'+
							'</div>'+
						'</div>'+
					'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
					'<div class="container1ffff">'+
						'<div class="col1ffff">'+
							'<div class = "input2" style = "color:white">mana</div>'+
						'</div>'+
						'<div class="col2ffff">'+
							'<input class = "input hpInput" id = "minMana" >'+
						'</div>'+
						'<div class="col3ffff">'+
							'<input class = "input hpInput" id = "maxMana" >'+
						'</div>'+
					'</div>'+
					'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
					'<div class="container1ffff">'+
						'<div class="col1ffff">'+
							'<div class = "input2" style = "color:white">attack</div>'+
						'</div>'+
						'<div class="col2ffff">'+
							'<input class = "input levelInput" id = "minAttack" >'+
						'</div>'+
						'<div class="col3ffff">'+
							'<input class = "input levelInput" id = "maxAttack" >'+
						'</div>'+
					'</div>'+
					'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
					'<div class="container1ffff">'+
						'<div class="col1ffff">'+
							'<div class = "input2" style = "color:white">defense</div>'+
						'</div>'+
						'<div class="col2ffff">'+
							'<input class = "input levelInput" id = "minDefense" >'+
						'</div>'+
						'<div class="col3ffff">'+
							'<input class = "input levelInput" id = "maxDefense" >'+
						'</div>'+
					'</div>'+
					'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
					'<div class="container1ffff">'+
						'<div class="col1ffff">'+
							'<div class = "input2" style = "color:white">agility</div>'+
						'</div>'+
						'<div class="col2ffff">'+
							'<input class = "input levelInput" id = "minAgility" >'+
						'</div>'+
						'<div class="col3ffff">'+
							'<input class = "input levelInput" id = "maxAgility" >'+
						'</div>'+
					'</div>'+
					'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
					'<div class="container1ffff">'+
						'<div class="col1ffff">'+
							'<div class = "input2" style = "color:white">range</div>'+
						'</div>'+
						'<div class="col2ffff">'+
							'<input class = "input levelInput" id = "minRange" >'+
						'</div>'+
						'<div class="col3ffff">'+
							'<input class = "input levelInput" id = "maxRange" >'+
						'</div>'+
					'</div>'+
					'</div>'+
					'</div>'+
					'<div class="container3ffff">'+
					'<div class="container2ffff">'+
					'<div class="container1ffff">'+
						'<div class="col1ffff">'+
							'<div class = "input2" style = "color:white">sp</div>'+
						'</div>'+
						'<div class="col2ffff">'+
							'<input class = "input levelInput" id = "minSp" >'+
						'</div>'+
						'<div class="col3ffff">'+
							'<input class = "input levelInput" id = "maxSp" >'+
						'</div>'+
					'</div>'+
					'</div>'+
					'</div>');
					*/
			
		}
		if(arg.player === true){
			gameData.wins += 1; alert("You won this round");
			if(gameData.wins == 1 && gameData.losses == 1) {
				alert("The game was drawn");
			}
			else if(gameData.wins == 2) {
				alert("You won the game");
			}
			else {
				//document.getElementById("pwins").innerHTML = "wins: 1";
				alert("beginning next round");
				beginNextRound();
			}
		}
		else{
			gameData.losses += 1; alert("You lost this round");
			if(gameData.wins == 1 && gameData.losses == 1) {
				alert("The game was drawn");
			}
			else if(gameData.losses == 2){
				alert("You lost the game");
			}
			else {
				//document.getElementById("owins").innerHTML = "wins: 1";
				alert("beginning next round"); 
				beginNextRound();
			}
		}
	},
	RowTargetUpdate:function(arg) {
		gameInterface.moveNextTarget(arg.row, (arg.column+1)%3, numberMaker(arg.player));
	},
	SwitchPositions:function(arg) {
		// MOVING (PERHAPS NOT MOVING HERE) DOES NOT ALWAYS RESTORE ONCLICK FUNCTIONS. (move and then target with any action skill the same engagement phase).
		var temp = gameData.fieldData[numberMaker(arg.player)][arg.a];
		gameData.fieldData[numberMaker(arg.player)][arg.a] = gameData.fieldData[numberMaker(arg.player)][arg.b];
		gameData.fieldData[numberMaker(arg.player)][arg.b] = temp;
		gameInterface.displayFieldCard(arg.a,numberMaker(arg.player));
		gameInterface.displayFieldCard(arg.b,numberMaker(arg.player));
	}
// CAN PEOPLE MOVE TO THE SAME LOCATION? DO I HAVE SAFEGUARDS TO CONTINUE WITHOUT PROMPTING IF NO MOVE IS AVAILABLE?
};

