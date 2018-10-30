var gameInterface = function() {
	var private = {
		thoughts:5,
		knowledge:[0,0,0,0,0],
		playerDOM:{0:"player",1:"enemy"},
		generateAttackOptions:function(position) {
			var range = gameData.fieldData[0][position].currentRange;
			for(var row = Math.floor((position)/3)-1; row > -1; --row) {
				if( ((gameData.fieldData[0][row*3].name != "no card")&&(gameData.fieldData[0][row*3].dead == false)) ||
						((gameData.fieldData[0][row*3 + 1].name != "no card")&&(gameData.fieldData[0][row*3 + 1].dead == false)) ||
						((gameData.fieldData[0][row*3 + 2].name != "no card")&&(gameData.fieldData[0][row*3 + 2].dead == false)) )
				{ --range; } }
			var rowsInRange = [false,false,false];
			for(var i = 0; i < 3 && range > 0; ++i) {					
				if(((gameData.fieldData[1][i*3].name != "no card")&&(gameData.fieldData[1][i*3].dead == false)) ||
					((gameData.fieldData[1][i*3 + 1].name != "no card")&&(gameData.fieldData[1][i*3 + 1].dead == false)) ||
					((gameData.fieldData[1][i*3 + 2].name != "no card")&&(gameData.fieldData[1][i*3 + 2].dead == false)))
				{ rowsInRange[i] = true; --range; } }
			return rowsInRange;
		},
		// These might be better handled in another way. I should perhaps not generate everything each time for instance.
		setToCardListHTML:function() { $(document.getElementById("cardList")).html(generateCardListHTML()); }, // NOT USING handgrave FOR CARD LIST ANYONE.
		
		updateGraveyardHTML:function(player){
			if(player === 0){
				for(var i = 0; i < gameData.graveyardData[0].length; ++i){
					$("#graveyardImage"+i).attr('src',gameData.graveyardData[0][i].imagehtml);
					document.getElementById("graveyard" + i).style.visibility = "visible";
				}
				for(var i = gameData.graveyardData[0].length; i < 25; ++i){
					document.getElementById("graveyard" + i).style.visibility = "hidden";
				}
			}
			else{
				for(var i = 0; i < gameData.graveyardData[1].length; ++i){
					$("#enemygraveyardImage"+i).attr('src',gameData.graveyardData[1][i].imagehtml);
					document.getElementById("enemygraveyard" + i).style.visibility = "visible";
				}
				for(var i = gameData.graveyardData[1].length; i < 25; ++i){
					document.getElementById("enemygraveyard" + i).style.visibility = "hidden";
				}
			}
		},
		
		updateHandHTML:function(player) {
			////$('#'+private.playerDOM[player] + "Hand").html(generateHandGraveHTML(gameData.handData[player],private.playerDOM[player]+"Hand"));
			
			if(player === 0){
				for(var i = 0; i < gameData.handData[0].length; ++i){
					$("#handImage"+i).attr('src',gameData.handData[0][i].imagehtml);
					document.getElementById("hand" + i).style.visibility = "visible";
				}
				for(var i = gameData.handData[0].length; i < 25; ++i){
					document.getElementById("hand" + i).style.visibility = "hidden";
				}
			}
			else{
				for(var i = 0; i < gameData.handData[1].length; ++i){
					$("#enemyhandImage"+i).attr('src',gameData.handData[1][i].imagehtml);
					document.getElementById("enemyhand" + i).style.visibility = "visible";
				}
				for(var i = gameData.handData[1].length; i < 25; ++i){
					document.getElementById("enemyhand" + i).style.visibility = "hidden";
				}
			}
			
		}, // ARGUMENTS HERE TO BE REFACTORED
		setToGraveyardHTML:function(player) { $('#'+private.playerDOM[player]+"Graveyard").html(generateHandGraveHTML(gameData.graveyardData[player],private.playerDOM[player]+"Graveyard")); },
		getHTMLfield:function(square) { return "<tr><td> <img src=\"/images/" + square + ".png\" /> </td></tr>"; }, // this function generates the html for empty square on the field at specified position.
		getHTMLfromImage:function(arg) { return '<tr>' + '<td><img src=' + arg + '/></td>' + '</tr>'; },
		
		beginUnitTurn:function(position,player) {
			if(private.currentUnit !== null) {
				private.endMove(private.playerTurn+private.currentUnit);
			}
			private.currentUnit = position;
			private.beginMove(player+private.currentUnit);
		},
		kill:function(arg) {
			$('#'+arg).find('.face').removeClass('front').addClass('back').removeClass('shadow').addClass('backRotate').removeClass('backflipshift');
			$('#'+arg).find('.backside').removeClass('back').addClass('front').removeClass('backRotate').removeClass('frontflipshift');
		},
		revive:function(arg) {
			$('#'+arg).find('.face').addClass('front').removeClass('back').removeClass("backRotate");
		    $('#'+arg).find('.backside').addClass('back').removeClass('front').addClass('backRotate');
		},
		beginMove:function(arg) {
			$('#'+arg).find('.face').addClass('backflipshift').addClass('front').removeClass('back').addClass('shadow').removeClass("backRotate");
			$('#'+arg).find('.backside').addClass('frontflipshift').addClass('back').removeClass('front').addClass("backRotate");
		},
		endMove:function(arg) {
			$('#'+arg).find('.face').removeClass('shadow').removeClass('backflipshift');//.removeClass("backRotate");
		    $('#'+arg).find('.backside').removeClass('frontflipshift');//.addClass("backRotate");
		},
		clearDisplaySetCard:function(player){
			if(player === 0){
				document.getElementById("playerSetCard").style.visibility = "hidden";
			}
			else{
				document.getElementById("enemySetCard").style.visibility = "hidden";
			}
		},
		clearDisplayFieldCard:function(index,player) {
			document.getElementById(private.playerDOM[player]+index+"Card").style.visibility = "hidden";
		},

		
		populatePlayerField:function(index,image) {
			$("#player"+index+"Image").attr("src",image);
			document.getElementById("player"+index+"Card").style.visibility = "visible";
			
		
		},
		populateEnemyField:function(index, image){
			$("#enemy"+index+"Image").attr("src",image);
			document.getElementById("enemy"+index+"Card").style.visibility = "visible";
		},
		
		
		// should I have an end turn update function? how should unit turns be handled?
		currentUnit:null,
		nextPlayerTarget:[0,0,0],
		nextEnemyTarget:[0,0,0]
	};
	var public = {
			gameLoaded:false,
		playerTurn:'enemy',/* PROBLEM WITH THIS BEING USED INCONSISTENTLY!!! IS IT true,false, or is it 'player','enemy'??? (here and in gameData)*/
		
		// for now don't have different colors for positive, negative stat changes, etc.
		// for now I am animating the movement with jquery rather than with css.
		animatePlayerSkill:function(callback, index, name){
			document.getElementById("statAnimationPlayer"+index).style.top = "0%";
			document.getElementById("statAnimationPlayer"+index).style.opacity = "1.0";
			document.getElementById("statAnimationPlayer"+index).innerHTML = name;
			$("#statAnimationPlayer"+index).animate({opacity:0.0,top:"10%"}, 1000, callback);
		},
		animateEnemySkill:function(callback, index, name){
			document.getElementById("statAnimationEnemy"+index).style.top = "0%";
			document.getElementById("statAnimationEnemy"+index).style.opacity = "1.0";
			document.getElementById("statAnimationEnemy"+index).innerHTML = name;
			$("#statAnimationEnemy"+index).animate({opacity:0.0,top:"10%"}, 1000, callback);
		},
		animatePlayerStatChange:function(callback, index, stat, value){
			document.getElementById("statAnimationPlayer"+index).style.top = "0%";
			document.getElementById("statAnimationPlayer"+index).style.opacity = "1.0";
			document.getElementById("statAnimationPlayer"+index).innerHTML = stat + ":" + value;
			$("#statAnimationPlayer"+index).animate({opacity:0.0,top:"10%"}, 1000, callback);
		},
		animateEnemyStatChange:function(callback, index, stat, value){
			document.getElementById("statAnimationEnemy"+index).style.top = "0%";
			document.getElementById("statAnimationEnemy"+index).style.opacity = "1.0";
			document.getElementById("statAnimationEnemy"+index).innerHTML = stat + ":" + value;
			$("#statAnimationEnemy"+index).animate({opacity:0.0,top:"10%"}, 1000, callback);
		},
		engagePlayerUnit:function(callback, index){
			document.getElementById("player"+index+"Engaged").style.visibility = "visible";
			callback();
		},
		disengagePlayerUnit:function(callback, index){
			document.getElementById("player"+index+"Engaged").style.visibility = "hidden";
			callback();
		},
		engageEnemyUnit:function(callback,index){
			document.getElementById("enemy"+index+"Engaged").style.visibility = "visible";
			callback();
		},
		disengageEnemyUnit:function(callback,index){
			document.getElementById("enemy"+index+"Engaged").style.visibility = "hidden";
			callback();
		},
		
		
		setThoughts:function(arg){
			private.thoughts = arg;
			public.updateThoughts();
		},
		setKnowledge:function(arg){
			for(var i = 0; i < 6; ++i){
				private.knowledge[i] = arg[i];
				public.updateKnowledge();
			}		
		},
			// temp
		getCurrentUnit:function(){return private.currentUnit;},
			
		move:function() {
			for(var i = 0; i < 9; ++i){
				if(gameData.fieldData[0][i].name == "no card"){
					break;
				}
				if(i == 8){
					post({type:"Move",end:-2});
					return;
				}
			}
			moveCurrently = true;
			document.getElementById("cardSearchAreaOtherMore").style.display = "none";
			var tempFunction = phase.clickPlayerField;
			phase.clickPlayerField = function(arg) {
				if(gameData.fieldData[0][arg].name != "no card"){
					// do nothing: you cannot move to an occupied square.
				}
				else {
					post({type:"Move",end:arg});
					phase.clickPlayerField = tempFunction;
					moveCurrently = false;
				}
			};
		},
		attack:function() {
			var rowsInRange = private.generateAttackOptions(private.currentUnit);
			for(var i = 0; i < 3; ++i) {
				if(rowsInRange[i] == true){
					document.getElementById("row" + i.toString()).style.visibility = 'visible';
				}		
			}
			if((rowsInRange[0] == false) && (rowsInRange[1] == false) && (rowsInRange[2] == false)) post({"type":"Attack","row":7});
			// 7 is the magic value if no row is available to attack.	 Perhaps this should be a separate update object.
		},
		generateActionList:function(position) {
			var allowDirectAttack = true, allowAttack = true, allowRest = true, allowMove = true, strActions = "";
			if(gameData.getThoughts(0) > 0) {
				for(var i = 0; i < 9; ++i) {
					if((gameData.fieldData[1][i].name != "no card") && (gameData.fieldData[1][i].dead == false)) {
						allowDirectAttack = false;
					}
				}
			}
			else {
				allowDirectAttack = false;
			}
			allowMove = false;
			for(var i = 0; i < 9; ++i) { if(gameData.fieldData[0][i].name == "no card") allowMove = true; }				
			if(allowDirectAttack) strActions += "<button id='directAttackAction'  onclick = 'userInput.directAttack()' class = 'myButton'>Direct Attack</button>";
			if(allowAttack) strActions += "<button id='attackAction'  onclick = 'gameInterface.attack()' class = 'myButton'>Attack</button>"; // if can actually attack this isn't an attack yet...
			if(allowRest) strActions += "<button id='restAction'  onclick = 'userInput.rest()' class = 'myButton'>Rest</button>";
			if(allowMove) strActions += "<button id='moveAction' onclick = 'gameInterface.move()' class = 'myButton'>Move</button>";

			// generate list of action skills
			for(var i = 0; i < gameData.fieldData[0][position].actionSkills.length; ++i) {	
				strActions += "<button class = 'myButton' onclick = 'userInput.initiateActionSkill("+i+");'>"+ gameData.fieldData[0][position].actionSkillsName[i] +"</button>";																											
			}
			document.getElementById('cardSearchAreaOtherMore').style.display = "block";
			$(document.getElementById('cardSearchAreaOtherMore')).html(strActions);
			updates.executeUpdates();
		},
		// used manually after end of fight phase. This isn't a good design.
		clearLastUnitTurn:function(){private.endMove(public.playerTurn+private.currentUnit);},
		unitTurn:function(position,player) {
			public.clearLastUnitTurn();
			private.beginUnitTurn(position,private.playerDOM[player]);
			if(player === 0){
				public.generateActionList(position)
			};
			public.playerTurn = private.playerDOM[player];
		},
		confirmSkillComponentSelection:function() {
			post({type:"SkillDataSpecification",positions:phase.selectedPositions});
			public.deselectSelected();
			phase.selectedPositions = [[],[],[],[],[],[],[],[]];
		},
		deselectSelected:function() {
			for(var i = 0; i < phase.selectedPositions[0].length; ++i) { public.deselectField(phase.selectedPositions[0][i],0); }
			for(var i = 0; i < phase.selectedPositions[1].length; ++i) { public.deselectField(phase.selectedPositions[1][i],1); }
			for(var i = 0; i < phase.selectedPositions[2].length; ++i) { public.deselectHand(phase.selectedPositions[2][i],0); } 
			for(var i = 0; i < phase.selectedPositions[3].length; ++i) { public.deselectHand(phase.selectedPositions[3][i],1); }
			for(var i = 0; i < phase.selectedPositions[4].length; ++i) { public.deselectGraveyard(phase.selectedPositions[4][i],0); }
			for(var i = 0; i < phase.selectedPositions[5].length; ++i) { public.deselectGraveyard(phase.selectedPositions[5][i],1); }
			for(var i = 0; i < phase.selectedPositions[6].length; ++i) { public.deselectSoul(phase.selectedPositions[6][i],0); }
			for(var i = 0; i < phase.selectedPositions[7].length; ++i) { public.deselectSoul(phase.selectedPositions[7][i],1); }
		},
		displaySet:function(player) {
			if(gameData.setData[player].name == "no card") {private.clearDisplaySetCard(player); }
			else { private.populateSet(gameData.setData[player].imagehtml); }	
		},
		displayFieldCard:function(arg,player) { 
// IS DISPLAY DETAILS BEING READDED WHERE IT SHOULD BE, AND NOT WHERE IT SHOULD NOT!?
			
			
			// SHOULD PERHAPS MAKE SURE THAT CARD IS NOT DISPLAYED AS BEING ON MOVE IF IT SHOULD NOT BE....................................
			// (THAT MIGHT NOT BELONG HERE THOUGH).
			console.log(arg);
			console.log(player);
			if(gameData.fieldData[player][arg].name == "no card"){
				private.clearDisplayFieldCard(arg,player);
				public.reviveField(arg, player);
			}
			else {
				if(player == 0){
					private.populatePlayerField(arg, gameData.fieldData[player][arg].imagehtml);
				}
				else {
					private.populateEnemyField(arg, gameData.fieldData[player][arg].imagehtml);
				}
				if(gameData.fieldData[player][arg].dead){
					public.killField(arg, player);
				}
				else{
					public.reviveField(arg, player);
				}
			}
		},
		setCard:function(player) {
			//$("#"+private.playerDOM[player]+"set").html(private.getHTMLfromImage(gameData.setData[player].imagehtml));
			//$("#"+private.playerDOM[player]+"set").addClass("displayDetails");
			if(player === 0){
				$("#playerSetImage").attr("src",gameData.setData[0].imagehtml);
				document.getElementById("playerSetCard").style.visibility = "visible";
			}
			else{
				$("#enemySetImage").attr("src",gameData.setData[1].imagehtml);
				document.getElementById("enemySetCard").style.visibility = "visible";
			}
		},
		setMinimallyDisplayedCard:function(arg) {
			var school = "";
			if(arg.school.length > 0) school += gameData.schools[arg.school[0]];
			for(var i = 1; i < arg.school.length; ++i) { school += " " + gameData.schools[arg.school[i]]; }
			$("#displayStats").html('<strong>' + arg.name + '<br />' + "Level: " + arg.level + '<br />' + "Schools of thought: " + '<br />' + school + 
			 '<br />' + '<br />' + 'Card Stats:' + '</strong>' + '<br />' +'HP: '+arg.hp+'<br />'
			+'Attack: '+arg.attack+'<br />'
			+'Defense: '+arg.defense+'<br />'
			+'Agility: '+arg.agility+'<br />'
			+'Range: '+arg.range+'<br />'
			+'Max Mana: '+arg.mana
			+ '<br />'+ "LP: " +arg.LP);
		},
		/*setDisplayedCard:function(arg) {	
			var school = "";
			for(var i = 0; i < arg.school.length-1; ++i) { school += arg.school[i] + " "; }
			if(arg.school.length > 0) school += arg.school[i];
			$("#displayStats").html('<strong>' + arg.name + '<br />' + "Level: " + arg.level + '<br />' + "Schools of thought: " + '<br />' + school + 
			'<br />' + '<br />' + 'Card Stats:' + '</strong>' + '<br />' +'HP: '+arg.currentHP+'<br />'
			+'Max HP: '+arg.currentMaxHP+'/'+arg.modifiedMaxHP+'/'+arg.baseMaxHP+'<br />'
			+'Attack: '+arg.currentAttack+'/'+arg.modifiedAttack+'/'+arg.baseAttack+'<br />'
			+'Defense: '+arg.currentDefense+'/'+arg.modifiedDefense+'/'+arg.baseDefense+'<br />'
			+'Agility: '+arg.currentAgility+'/'+arg.modifiedAgility+'/'+arg.baseAgility+'<br />'
			+'Range: '+arg.currentRange+'/'+arg.modifiedRange+'/'+arg.baseRange+'<br />'
			+'Mana: '+arg.currentMana+'/'+arg.baseMana+'<br />'
			+'Max Mana: '+arg.currentMaxMana+'/'+arg.baseMaxMana
			+ '<br />' + "Turns Engaged: " + arg.engaged
			+ '<br />'+ "LP: " +arg.LP + '/' + arg.maxLP);
		},*/
		print:function(arg) {
			/*
			gameInterface.messagetext += arg; gameInterface.messagetext += "\n"; gameInterface.messagetext += "--------------------------------";
			gameInterface.messagetext += "\n"; $(document.getElementById('log')).html(gameInterface.messagetext); $('#log').scrollTop($('#log')[0].scrollHeight);
			*/
		},
		select2:function(arg) {
			console.error('#' + arg);
			$('#' + arg).removeClass("unselected").addClass("selected");
		},
		deselect2:function(arg) { $('#' + arg).removeClass("selected").addClass("unselected"); },
		select:function(arg,prefix) {
			console.error("here3");
			if(prefix === true)
				return public.select2(arg);
			console.error(arg);
			$('#' + arg).find('.unselected').removeClass('unselected').addClass('selected');
		},
		deselect:function(arg,prefix) { if(prefix === true) return public.deselect2(arg); $('#' + arg).find('.selected').removeClass('selected').addClass('unselected');},
			
		selectField:function(index,player) {
			$('#'+private.playerDOM[player] + index).find('.unselected').removeClass('unselected').addClass('selected');
		},
		selectHand:function(index,player) {
			if(player === 0){
				$("#hand"+index).removeClass("unselected").addClass("selected");
			}
			else{
				$("#enemyhand"+index).removeClass("unselected").addClass("selected");
			}
			///return $('#'+private.playerDOM[player]+'Hand' + index).removeClass("unselected").addClass("selected");
		},
		selectGraveyard:function(index,player) { return $('#'+private.playerDOM[player]+'Graveyard' + index).removeClass("unselected").addClass("selected"); },
		selectSoul:function(index,player) { $('#'+private.playerDOM[player]+'Soul' + index).find('.unselected').removeClass('unselected').addClass('selected'); },
		deselectField:function(index,player) { $('#'+private.playerDOM[player]+ index).find('.selected').removeClass('selected').addClass('unselected'); },
		deselectHand:function(index,player) {
			
			if(player === 0){
				$("#hand"+index).removeClass("selected").addClass("unselected");
			}
			else{
				$("#enemyhand"+index).removeClass("selected").addClass("unselected");
			}
			
			//return $('#'+private.playerDOM[player]+'Hand' + index).removeClass("selected").addClass("unselected");
		},
		deselectGraveyard:function(index,player) { return $('#'+private.playerDOM[player]+'Graveyard' + index).removeClass("selected").addClass("unselected"); },
		deselectPlayerSoul:function(index) { $('#'+private.playerDOM[0]+'Soul' + index).find('.selected').removeClass('selected').addClass('unselected'); },
		deselectSoul:function(index,player) { $('#'+private.playerDOM[player]+'Soul' + index).find('.selected').removeClass('selected').addClass('unselected'); },
		// this next function is here for now. It's for the card list.
		selectCard:function(index) {
			if(index !== public.selectedCardId) {
				public.deselect("cardList"+public.selectedCardId);
				public.select("cardList"+index);
				public.selectedCardId = cardList.cardIds[index];
				public.selectedCardId = index;
			}
			else {
				public.deselect("cardList"+index);
				public.selectedCardId = "";
			}
		},
		
		instruct:function(arg) { $(document.getElementById('currentAction')).html(arg); },
		updateLifePoints:function() { /*
			$(lp).html("<tr><td>" + gameData.LP + "</td></tr>"); $(enemylp).html("<tr><td>" + gameData.enemyLP + "</td></tr>");
			*/
		},
		
		
		
		
		
		
		populatePlayerSoul:function(index,image) {
			$("#playerSoulImage"+index).attr("src",image);
			document.getElementById("playerSoulFace"+index).style.display = "block";
			document.getElementById("playerSoulBack"+index).style.display = "block";
			populate(index, gameData.soulData[0][index].LP);
			

			/*
			$('#'+private.playerDOM[player]+index).html("<div class = 'unselected' style='width:72px'><img src=/images/"+index+".png/></div>" +
				"<div class='face front card unselected'> <img src="+
				image+"/></div><div class='backside backRotate back card unselected'> <img src=/images/cardback.png /></div>");
				*/
		},
		populateEnemySoul:function(index, image){
			$("#enemySoulImage"+index).attr("src",image);
			document.getElementById("enemySoulFace"+index).style.display = "block";
			document.getElementById("enemySoulBack"+index).style.display = "block";
			enemyPopulate(index, gameData.soulData[1][index].LP);
		},
		
		
		
		populateSoul:function(index,image,player) {
			$('#'+private.playerDOM[player]+'Soul'+index).html("<div class = 'unselected' style='width:72px'>" +
					"<img src=/images/soul.png/></div><div class='face front card unselected'>" +
					"<img src="+image+"/></div><div class='backside backRotate back card unselected'> <img src=/images/cardback.png/></div>");
		},
		killSoul:function(index,player) {
			//private.kill(private.playerDOM[player]+'Soul'+index);
			if(player === 0){
				$('#playerSoul' + index).find('.face').removeClass('front').addClass('back').removeClass('shadow').addClass('backRotate').removeClass('backflipshift');
				$('#playerSoul' + index).find('.backside').removeClass('back').addClass('front').removeClass('backRotate').removeClass('frontflipshift');
			}
			else{
				$('#enemySoul' + index).find('.face').removeClass('front').addClass('back').removeClass('shadow').addClass('backRotate').removeClass('backflipshift');
				$('#enemySoul' + index).find('.backside').removeClass('back').addClass('front').removeClass('backRotate').removeClass('frontflipshift');
			}
		},
		restoreSoul:function(index,player) {
			//private.kill(private.playerDOM[player]+'Soul'+index);
			if(player === 0){
				$('#playerSoul' + index).find('.face').addClass('front').removeClass('back').addClass('shadow').removeClass('backRotate').remove('backflipshift');
				$('#playerSoul' + index).find('.backside').addClass('back').removeClass('front').addClass('backRotate').remove('frontflipshift');
			}
			else{
				$('#enemySoul' + index).find('.face').addClass('front').removeClass('back').addClass('shadow').removeClass('backRotate').remove('backflipshift');
				$('#enemySoul' + index).find('.backside').addClass('back').removeClass('front').addClass('backRotate').remove('frontflipshift');
			}
		},
		beginMoveSoul:function(index) { private.beginMove(private.playerDOM[player]+'Soul'+index); },
		endMoveSoul:function(index) { private.endMove(private.playerDOM[player]+'Soul'+index); },
		killField:function(index,player){private.kill(private.playerDOM[player]+index);},
		reviveField:function(index,player){private.revive(private.playerDOM[player]+index);},
		beginMoveField:function(index){private.beginMove(private.playerDOM[player]+index);},
		endMoveField:function(index){private.endMove(private.playerDOM[player]+index);},
		displayCardList:function() {
			/*
			document.getElementById("playerField").style.visibility = 'hidden';
			document.getElementById("playerHandWrapper").style.visibility = 'hidden';
			document.getElementById("playerGraveyardWrapper").style.visibility = 'hidden';
			document.getElementById("cardListWrapper").style.visibility = 'visible';
			private.setToCardListHTML();
			handSelected = false;
			var allValues = [];
			for(var i = 0; i < phase.selectedPositions[2].length; ++i) { allValues.push(phase.selectedPositions[2][i]); }
			for(var i = 0; i < allValues.length; ++i) { public.select(allValues[i]); }
			document.getElementById("playerFieldRow0").style.visibility = 'hidden';
			document.getElementById("playerFieldRow1").style.visibility = 'hidden';
			document.getElementById("playerFieldRow2").style.visibility = 'hidden';
			if(phase.getName() === 'draw') {
				document.getElementById("drawCardButton").style.visibility = 'visible';
				document.getElementById("handButton").style.visibility = 'visible';
				document.getElementById("browseCardsButton").style.visibility = 'hidden';
			}
			*/
			//cards[3*i+j].imagehtml
			for(var i = 0; i < cards.length; ++i){
				document.getElementById("cardList" + i).style.visibility = "visible";
			}
			for(var i = cards.length; i < 25; ++i){
				document.getElementById("cardList" + i).style.visibility = "hidden";
			}
		},
		displayField:function(player) {
			if(player === 0){
				selectPlayerField();
			}
			else{
				public.displayEnemyField();
			}
		},
		updateGraveyard:function(player){
			//private.setToGraveyardHTML(player);
			private.updateGraveyardHTML(player);
		},
		displayGraveyard:function(player) {
			if(player === 0){
				selectPlayerGraveyard();
			}
			else{
				console.error("select enemy graveyard not implemented");
			}
		},
		updateHand:function(player){
			private.updateHandHTML(player);
		},
		displayHand:function() {
			document.getElementById("playerField").style.visibility = 'hidden';
			document.getElementById("playerHandWrapper").style.visibility = 'visible';
			private.updateHandHTML();
			handSelected = true;
			var allValues = [];
			for(var i = 0; i < phase.selectedPositions[2].length; ++i) { allValues.push(phase.selectedPositions[2][i]); }
			for(var i = 0; i < allValues.length; ++i) { public.select(allValues[i]); }
			document.getElementById("playerFieldRow0").style.visibility = 'hidden';
			document.getElementById("playerFieldRow1").style.visibility = 'hidden';
			document.getElementById("playerFieldRow2").style.visibility = 'hidden';
			if(phase.getName() === 'draw') {
				document.getElementById("drawCardButton").style.visibility = 'hidden';
				document.getElementById("handButton").style.visibility = 'hidden';
				document.getElementById("browseCardsButton").style.visibility = 'visible';
			}
		},
		displayEnemyField:function() {
			selectEnemyField();
		},
		displayEnemyGraveyard:function() {
			document.getElementById("enemyField").style.visibility = 'hidden';
			document.getElementById("enemyHandWrapper").style.visibility = 'hidden';
			document.getElementById("enemyGraveyardWrapper").style.visibility = 'visible';
			private.setToGraveyardHTML(1);
			document.getElementById("enemyFieldRow0").style.visibility = 'hidden';
			document.getElementById("enemyFieldRow1").style.visibility = 'hidden';
			document.getElementById("enemyFieldRow2").style.visibility = 'hidden';
		},
		displayEnemyHand:function() {
			/*
			document.getElementById("enemyField").style.visibility = 'hidden';
			document.getElementById("enemyGraveyardWrapper").style.visibility = 'hidden';
			document.getElementById("enemyHandWrapper").style.visibility = 'visible';
			private.setToHandHTML(1);
			document.getElementById("enemyFieldRow0").style.visibility = 'hidden';
			document.getElementById("enemyFieldRow1").style.visibility = 'hidden';
			document.getElementById("enemyFieldRow2").style.visibility = 'hidden';
			*/
		},
		
		
		moveNextTarget:function(row,column,player){
			if(player === 0){
				public.moveNextPlayerTarget(row,column);				
			}
			else{
				public.moveNextEnemyTarget(row,column);
			}
		},
		moveNextPlayerTarget:function(row,column) {
			var index = row*3 + column;
			if(column != private.nextPlayerTarget[row]){
				$("#playerNextTarget"+(row*3+private.nextPlayerTarget[row])).fadeOut( "slow", function(){});
				$("#playerNextTarget"+index).fadeIn( "slow", function(){});
			}
			private.nextPlayerTarget[row] = column;
		},
		moveNextEnemyTarget:function(row,column) {
			var index = row*3 + column;
			if(column != private.nextEnemyTarget[row]){
				$("#enemyNextTarget"+(row*3+private.nextEnemyTarget[row])).fadeOut( "slow", function(){});
				$("#enemyNextTarget"+index).fadeIn( "slow", function(){});
			}
			private.nextEnemyTarget[row] = column;
		},
		
		
		
		updateThoughts:function(arg) { // thoughts is somewhere in global scope as well.
			//$(document.getElementById('thoughtCount')).html("<tr> <td>" + gameData.tempThoughtCount + "</td> </tr>");
			
			document.getElementById("playerThoughtSpan").innerHTML = arg;
		},
		updateKnowledge:function(index,value,staleValue) {
			$('#ball' + (index+1)).removeClass("value-"+staleValue).addClass("value-"+value);
		},
		updateEnemyKnowledge:function(index,value,staleValue) {
			$('#enemyBall' + (index+1)).removeClass("value-"+staleValue).addClass("value-"+value);	
		},
		updateEnemyThoughts:function(arg) {
			document.getElementById("enemyThoughtSpan").innerHTML = arg;
		},	
		swapSelected:function(target) {
			if($("#"+(target)).hasClass("selected")) $("#"+(target)).removeClass("selected").addClass("unselected");
			else $("#"+(target)).removeClass("unselected").addClass("selected");
		},
		
		reset:function(){
			
			
			document.getElementById("playerThoughtSpan").innerHTML = "";
			$("#playerThoughtFigure").addClass("ball-thoughts-text");
			
			private.clearDisplaySetCard(0);
			private.clearDisplaySetCard(1);
			for(var i = 0; i < 9; ++i){
				private.clearDisplayFieldCard(i, 1);
				private.clearDisplayFieldCard(i, 0);
			}
			for(var i = 0; i < 9; ++i){
				public.disengagePlayerUnit(function(){}, i);
				public.disengageEnemyUnit(function(){}, i);
			}
			
			
			// not clearing soul yet.
			private.updateHandHTML(0);
			private.updateHandHTML(1);
			// need to update graveyardHTML too.
			private.updateGraveyardHTML(0);
			private.updateGraveyardHTML(1);
			
			
			
			for(var i = 1; i < 7; ++i){
				$("#ball"+i).removeClass("value-0");
				$("#ball"+i).removeClass("value-1");
				$("#ball"+i).removeClass("value-2");
				$("#ball"+i).removeClass("value-3");
				$("#ball"+i).removeClass("value-4");
				$("#ball"+i).removeClass("value-5");
				$("#ball"+i).removeClass("value-6");
				$("#ball"+i).removeClass("value-7");
				$("#ball"+i).removeClass("value-8");
				$("#ball"+i).removeClass("value-9");
			}
			document.getElementById("playIcons").style.display = "none";
			document.getElementById("container57777").style.display = "block";
			
			$(".plus-button").each(function(index,element) {
				$(element).css("display","none");
			});
			$(".minus-button").each(function(index,element) {
				$(element).css("display","none");
			});
			
			
			private.nextPlayerTarget = [0,0,0];
			private.nextEnemyTarget = [0,0,0];
			
			
			selectCardList();
			updates.executeUpdates();
			drawPhaseOngoing = true;
			console.log("beginning draw again!!");
		},
		
		
		// temporary functions
		getSoulIndex:function(){ if(phase.selectedPositions[6].length === 1) return phase.selectedPositions[6][0]; return -1;},
		
		
		
		selectedCardId:"" // this could perhaps be moved to the draw phase....
	};
	
//	function clickSoul(arg) {
//		if(soulIndex != arg) { gameInterface.deselect("soul"+soulIndex); soulIndex = arg; gameInterface.select("soul"+soulIndex); }
//		else { gameInterface.deselect("soul"+soulIndex); soulIndex = -1; } }

	return public;
}();
