var phase = function() {
	var private = {
		deselectPhaseName:function(arg) { /*document.getElementById(arg).style.backgroundColor = "#FFFFFF";*/ },
		selectPhaseName:function(arg) { /*document.getElementById(arg).style.backgroundColor="#45DC1F"; */},
		registerClick:function(container, position, interfacePrefix, prefix) {
			//if(clock.playerTurn){
				if(public.selectedPositions[container].contains(position)) {
					gameInterface.deselect(interfacePrefix + "" + position,prefix);
					public.selectedPositions[container].remove(position);
				}
				else {
					gameInterface.select(interfacePrefix+""+position,prefix);
					public.selectedPositions[container].push(position);
				}
			//}
		},
		drawPhase:(function() {
			var startIndex = 0; /* for requesting partial card lists from the server */
			var name = "draw"; var soulIndex = -1; var cardName = null; var cardIndex = 0; /* for interface book keeping */
			function endPhase() {
				/*
				for(var i = 0; i < 5; ++i)
					gameInterface.deselect("soul"+i);
				$(".hideAfterDrawPhase").each(function() {
					this.style.visibility = "hidden";
				});
				$(".showAfterDrawPhase").each(function() {
					this.style.visibility = "visible";
				});
				$("#nondisplaycard").html("");
				gameInterface.displayField();
				gameInterface.print("draw phase end");
				*/
				
				
				
				resetSearchSpecification();
				private.deselectAll();
				console.log("endDraw");
				endDraw();
				
				
			}
			function gotoNextPhase() { endPhase(); private.currentPhase = private.setPhase; }
			
			function userTurn(player){
				if(player){
					
				}
				else{
					
				}
			}
			
			return {name:name,endPhase:endPhase,gotoNextPhase:gotoNextPhase,userTurn:userTurn};
		})(),
		setPhase:(function() {	
			var name = "set";
			function beginPhase() {

				
				++gameData.turnNumber;
				document.getElementById("turnNumberDiv").innerHTML = "Turn: " + gameData.turnNumber;

				
				
				$(".plus-button").each(function(index,element) {
					$(element).css("visibility","visible");
				});
				$(".minus-button").each(function(index,element) {
					$(element).css("visibility","visible");
				});
				private.deselectAll();
				updates.executeUpdates();
			}
			function endPhase() {
				document.getElementById("setCardButton").style.display = "none";
				document.getElementById("skipButton").style.display = "none";
				$(".plus-button").each(function(index,element) {
					$(element).css("visibility","hidden");
				});
				$(".minus-button").each(function(index,element) {
					$(element).css("visibility","hidden");
				});
				private.deselectAll();
			}
			function gotoNextPhase() {
				private.currentPhase = private.openPhase;
				// Don't disable the buttons to change knowledge yet, but I do want to make sure that I reset the display to the permanent values
				gameData.removeTemporaryKnowledgeDisplay();
			}
			
			function userTurn(player){
				if(player){
					document.getElementById("setCardButton").style.display = "block";
					document.getElementById("skipButton").style.display = "block";
				}
				else{
					document.getElementById("setCardButton").style.display = "none";
					document.getElementById("skipButton").style.display = "none";
				}
			}
			
			return {name:name,beginPhase:beginPhase,endPhase:endPhase,gotoNextPhase:gotoNextPhase,userTurn:userTurn};
		})(),
		openPhase:(function() { var name = "open"; function gotoNextPhase() { private.currentPhase = private.removalPhase; } return {name:name,gotoNextPhase:gotoNextPhase}; })(),
		removalPhase:(function() { var name = "removal"; function gotoNextPhase() { private.currentPhase = private.startPhase; } return {name:name,gotoNextPhase:gotoNextPhase}; })(),
		startPhase:(function() { var name = "start"; function gotoNextPhase() { private.currentPhase = private.fightPhase; } return {name:name,gotoNextPhase:gotoNextPhase}; })(),
		fightPhase:(function(){
			var name = "fight";
			function endPhase() {
				document.getElementById('cardSearchAreaOtherMore').style.display = "none";
				private.deselectAll();
				gameInterface.clearLastUnitTurn();
			};
			function gotoNextPhase() { private.currentPhase = private.endPhase; };
			
			function userTurn(player){
				if(player && (skillCurrently === false) && (moveCurrently === false)){
					document.getElementById('cardSearchAreaOtherMore').style.display = "block";
				}
				else{
					document.getElementById('cardSearchAreaOtherMore').style.display = "none";
				}
			}
			
			
			return {name:name,endPhase:endPhase,gotoNextPhase:gotoNextPhase,userTurn:userTurn};
		})(),
		endPhase:(function() { var name = "end"; function gotoNextPhase() { private.currentPhase = private.revivalPhase; } return {name:name,gotoNextPhase:gotoNextPhase}; })(),
		revivalPhase:(function (){
			var name = "revival";
			function beginPhase(){
				document.getElementById('confirmReviveButton').style.display = 'block';
				updates.executeUpdates();
			}
			function endPhase() {
				document.getElementById('confirmReviveButton').style.display = 'none';
				private.deselectAll();
			}
			function gotoNextPhase() {
				private.currentPhase = private.placePhase;
			}
			
			function userTurn(player){
				if(player){
					document.getElementById('confirmReviveButton').style.display = 'block';
				}
				else{
					document.getElementById('confirmReviveButton').style.display = 'none';
				}
			}
			
			return {name:name,beginPhase:beginPhase,endPhase:endPhase,gotoNextPhase:gotoNextPhase,userTurn:userTurn};
		})(),
		placePhase:(function() {
			var name = "place";
			function beginPhase() {
				document.getElementById('deployCardButton').style.display = 'block';
				updates.executeUpdates();
			}
			function endPhase() {
				document.getElementById('deployCardButton').style.display = 'none';
				private.deselectAll();
			}
			function gotoNextPhase() { private.currentPhase = private.setPhase; }	
			function userTurn(player){
				if(player){
					document.getElementById('deployCardButton').style.display = 'block';
				}
				else{
					document.getElementById('deployCardButton').style.display = 'none';
				}
			}
			return {name:name,beginPhase:beginPhase,endPhase:endPhase,gotoNextPhase:gotoNextPhase,userTurn:userTurn};
		})(),
		deselectAll:function(){
			console.log("logging selected:");
			console.log(public.selectedPositions);
			gameInterface.deselectSelected();
			for(var i = 0; i < 8; ++i){
				public.selectedPositions[i].length = 0;
			}
		}		
	};　private.currentPhase = private.drawPhase;
	var public = {
		registerClick:function(container, position, interfacePrefix, prefix) {
			if(public.selectedPositions[container].contains(position)) {
				gameInterface.deselect(interfacePrefix + "" + position,prefix);
				public.selectedPositions[container].remove(position);
			}
			else {
				gameInterface.select(interfacePrefix+""+position,prefix);
				public.selectedPositions[container].push(position);
			}
		},
		gotoNextPhase:function() {
			if(private.currentPhase.hasOwnProperty("endPhase")) { private.currentPhase.endPhase(); }
			if(private.currentPhase.name != 'draw') { private.deselectPhaseName(private.currentPhase.name + "Phase"); }
			//gameInterface.deselectSelected();
			//public.selectedPositions = [[],[],[],[],[],[],[],[]];
			private.currentPhase.gotoNextPhase();
			private.selectPhaseName(private.currentPhase.name + "Phase");
			if(private.currentPhase.hasOwnProperty("beginPhase")) {
				private.currentPhase.beginPhase();
			}
		},
		clickPlayerField:function(position) {
			if(private.currentPhase.name === "draw" || private.currentPhase.name === "set"){
				return;
			}
			if(private.currentPhase.name === "place" && phase.selectedPositions[0].length > 0 && position !== phase.selectedPositions[0][0]) {
				private.registerClick(0, phase.selectedPositions[0][0], "player",false);
			}
			private.registerClick(0, position, "player",false);
		},
		clickEnemyField:function(position) {
			if(private.currentPhase.name === "draw" || private.currentPhase.name === "set" || private.currentPhase.name === "revival" || private.currentPhase.name === "place"){
				return;
			}
			private.registerClick(1, position, "enemy",false);
		},
		clickPlayerHand:function(position) {
			if(private.currentPhase.name === "draw" || private.currentPhase.name === "revival" || private.currentPhase.name === "place"){
				return;
			}
			selectHandCard(position);
		},
		clickEnemyHand:function(position) {
			console.log(private.currentPhase.name);
			if(private.currentPhase.name === "draw" || private.currentPhase.name === "set" || private.currentPhase.name === "revival" || private.currentPhase.name === "place"){
				return;
			}
			selectEnemyHandCard(position);
		},
		clickPlayerGrave:function(position) {
			if(private.currentPhase.name === "draw" || private.currentPhase.name === "set" || private.currentPhase.name === "revival" || private.currentPhase.name === "place"){
				return;
			}
			private.registerClick(4, position, "playerGrave",true);
		},
		clickEnemyGrave:function(position) {
			if(private.currentPhase.name === "draw" || private.currentPhase.name === "set" || private.currentPhase.name === "revival" || private.currentPhase.name === "place"){
				return;
			}
			private.registerClick(5, position, "enemyGrave",true);
		},
		clickPlayerSoul:function(position){
			if(gameData.cardsDrawn[0] % 6 === 5){
				$("#playerSoulContainer"+position).toggleClass("unselected").toggleClass("selected");
				$("#playerSoulFace"+position).toggleClass("unselected").toggleClass("selected");
				$("#playerSoulBack"+position).toggleClass("unselected").toggleClass("selected");
				// handle data presentation later.  (CURRENTLY FOR THIS AND ENEMY FUNCTION NOT REPRESENTING WHOLE ARRAY PROPERLY..
				if(public.selectedPositions[6][0] !== position){
					if(public.selectedPositions[6][0] !== -1){
						$("#playerSoulContainer"+public.selectedPositions[6][0]).toggleClass("unselected").toggleClass("selected");
						$("#playerSoulFace"+public.selectedPositions[6][0]).toggleClass("unselected").toggleClass("selected");
						$("#playerSoulBack"+public.selectedPositions[6][0]).toggleClass("unselected").toggleClass("selected");
					}
					public.selectedPositions[6][0] = position;
				}
				else{
					public.selectedPositions[6][0] = -1;
				}
			}
		},
		clickEnemySoul:function(position){
			/*$("#enemySoulContainer"+position).toggleClass("unselected").toggleClass("selected");
			$("#enemySoulFace"+position).toggleClass("unselected").toggleClass("selected");
			$("#enemySoulBack"+position).toggleClass("unselected").toggleClass("selected");
			// handle data presentation later.
			if(public.selectedPositions[7][0] !== position){
				if(public.selectedPositions[7][0] !== -1){
					$("#enemySoulContainer"+public.selectedPositions[7][0]).toggleClass("unselected").toggleClass("selected");
					$("#enemySoulFace"+public.selectedPositions[7][0]).toggleClass("unselected").toggleClass("selected");
					$("#enemySoulBack"+public.selectedPositions[7][0]).toggleClass("unselected").toggleClass("selected");
				}
				public.selectedPositions[7][0] = position;
			}
			else{
				public.selectedPositions[7][0] = -1;
			}*/
		},
		//clickPlayerSoul:function(position) { private.registerClick(6, position, "playerSoul",false); },
		//clickEnemySoul:function(position) { private.registerClick(7, position, "enemySoul",false); },
		getName:function() { return private.currentPhase.name; },
		selectCard:function(arg) { if(private.currentPhase.hasOwnProperty("selectCard")) private.currentPhase.selectCard(arg);  },
		getName:function() { return private.currentPhase.name; },
		selectedPositions:[[],[],[],[],[],[],[],[]], // [playerField,enemyField,playerHand,enemyHand,playerGraveyard,enemyGraveyard,playerSoul,enemySoul]
		reset:function(){
			private.currentPhase = private.drawPhase;
			public.selectedPositions = [[],[],[],[],[],[],[],[]]; // not deselecting in the display yet.
		},
		userTurn:function(player){
			if(private.currentPhase.userTurn !== undefined){
				private.currentPhase.userTurn(player);
			}
		}
	};
	return public;
}();
