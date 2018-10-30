var gameData = (function gameData() {
	var public = {
		wins:0,
		losses:0,
		turnNumber:-1,
		cardsDrawn:[0,0],
		goFirst:false,
		gameStarted:false,
		gameOver:false,
		LP:[1,1],
		fieldData:[[new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData()],
		      	  [new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData()]],
		setData:[new NoCardData(),new NoCardData()],
		soulData:[[],[]],
		handData:[[],[]],
		graveyardData:[[],[]],
		
		setKnowledge:function(arg,player) {
			if(player === 0){
				private.setPlayerKnowledge(arg);			
			}
			else{
				private.setEnemyKnowledge(arg);			
			}
		},
		removeFromHand:function(index,player) {
			for(var i = index; i < public.handData[player].length; ++i) {
				if(i+1 < public.handData[player].length) {
					public.handData[player][i] = public.handData[player][i+1];
				}
			}
			public.handData[player].pop();
			gameInterface.updateHand(player);
		},
	    removeFromGraveyard:function(index,player) {
	    	for(var i = index; i < public.graveyardData[player].length; ++i) {
	    		if(i+1 < public.graveyardData[player].length) {
	    			public.graveyardData[player][i] = public.graveyardData[player][i+1];
	    		}
	    	}
	    	public.graveyardData[player].pop();
	    },
		addToHand:function(card, player) {
			public.handData[player].push(card);
			gameInterface.updateHand(player);
		},
		addToGraveyard:function(card, toIndex, player) {
			public.graveyardData[player].splice(toIndex,0,card);
			gameInterface.updateGraveyard(player);
		},
		takeLifeDamage:function(arg,player,callback) {
			if(!public.gameOver) // this check might not go well here, but put it here for now.
			{
				if(player === 0){
					damageSoul(arg);
				}
				else{
					damageEnemySoul(arg);
				}
				public.LP[player] -= arg;
				if(public.LP[player] < 0){
					public.LP[player] = 0;
				}
				for(var i = 0; i < 5; ++i) {
					if(public.soulData[player][i] === undefined){
						break; // means we are playing a minigame for testing purposes.
					}
					if(arg >= public.soulData[player][i].LP) {
						arg -= public.soulData[player][i].LP;
						//public.soulData[player][i].LP = 0;
						gameInterface.killSoul(i,player);
						if(arg === 0){
							break; // this condition added in case I'm playing a minigame without allocating all soul cards.
						}
					}
					else {
						//public.soulData[player][i].LP -= arg;
						arg = 0;
						break;
					}
				}
			}

		function decrementPlayerLP(){
			for(var i = 0; i < 5; ++i){
				if(public.soulData[0][i].LP > 0){
					--public.soulData[0][i].LP;
					if(public.soulData[0][i].LP === 0){
						gameInterface.killSoul(i,0);
					}
					return;
				}
			}
			console.log("lost last LP");
		}
		function decrementEnemyLP(){
			for(var i = 0; i < 5; ++i){
				if(public.soulData[1][i].LP > 0){
					--public.soulData[1][i].LP;
					if(public.soulData[1][i].LP === 0){
						gameInterface.killSoul(i,1);
					}
					return;
				}
			}
			console.log("opponent lost last LP");
		}
		
		function damageSoul(arg){
			if(arg == 0){
				callback();
				return;
			}
			var index;
			for(var i = 0; i < 5; ++i){
				if(public.soulData[0][i].LP > 0){
					index = i;
					break;
				}
				if(i == 4){
					index = -1;
				}
			}
			if(index != -1){
				$("#LP"+index+"-"+public.soulData[0][index].maxLP+"-"+public.soulData[0][index].LP).animate({width:"0%"},1000,function(){
					decrementPlayerLP();
					damageSoul(arg-1);
				});
			}
			else{
				console.log("lost last LP");
			}
		}


		function damageEnemySoul(arg){
			if(arg == 0){
				callback();
				return;
			}
			var index;
			for(var i = 0; i < 5; ++i){
				if(public.soulData[1][i].LP > 0){
					index = i;
					break;
				}
				if(i == 4){
					index = -1;
				}
			}
			if(index != -1){
				$("#enemyLP"+index+"-"+public.soulData[1][index].maxLP+"-"+public.soulData[1][index].LP).animate({width:"0%"},1000,function(){
					decrementEnemyLP();
					damageEnemySoul(arg-1);
				});
			}
			else{
				console.log("opponent lost last LP");
			}
		}
			
			
			
			
			
			
		},		
		reviveCard:function(index,player) {
			private.reviveCard(index,player);
			gameInterface.reviveField(index,player);
			if(player === 0){
				gameInterface.disengagePlayerUnit(function(){}, index);
			}
			else{
				gameInterface.disengageEnemyUnit(function(){}, index);
			}
		},
		clickedIncrementKnowledge:function(index){
			if(private.tempKnowledge[index] === 9 || private.tempThoughts === 0){
				return;
			}
			gameInterface.updateKnowledge(index,private.tempKnowledge[index]+1,private.tempKnowledge[index]);
			++private.tempKnowledge[index];
			--private.tempThoughts;
			gameInterface.updateThoughts(private.tempThoughts);
		},
		clickedDecrementKnowledge:function(index){
			console.error(index);
			console.error(private.tempKnowledge[index]);
			console.error(private.knowledge[0][index]);
			if(private.tempKnowledge[index] === private.knowledge[0][index]){
				return;
			}
			gameInterface.updateKnowledge(index,private.tempKnowledge[index]-1,private.tempKnowledge[index]);
			--private.tempKnowledge[index];
			++private.tempThoughts;
			gameInterface.updateThoughts(private.tempThoughts);
		},
		getKnowledge:function(player,index){
			return private.knowledge[player][index];
		},
		getAllKnowledge:function(player){
			return [private.knowledge[player][0],private.knowledge[player][1],private.knowledge[player][2],private.knowledge[player][3],private.knowledge[player][4],private.knowledge[player][5]];
		},
		getAllTempKnowledge:function(){
			return [private.tempKnowledge[0],private.tempKnowledge[1],private.tempKnowledge[2],private.tempKnowledge[3],private.tempKnowledge[4],private.tempKnowledge[5]];
		},
		setKnowledge:function(player,array){
			if(player === 0){
				for(var i = 0; i < 6; ++i){
					gameInterface.updateKnowledge(i,array[i],private.tempKnowledge[i]);
				}
				private.knowledge[0] = array;
				private.tempKnowledge = private.knowledge[0].slice(0);
			}
			else{
				for(var i = 0; i < 6; ++i){
					gameInterface.updateEnemyKnowledge(i,array[i],private.knowledge[1][i]);
				}
				private.knowledge[1] = array;
			}
		},
		getThoughts:function(player){
			return private.thoughts[player];
		},
		setThoughts:function(player,value){
			private.thoughts[player] = value;
			if(player === 0){
				gameInterface.updateThoughts(value);
				private.tempThoughts = private.thoughts[0] = value;
			}
			else{
				gameInterface.updateEnemyThoughts(value);
					private.thoughts[1] = value;
			}
		},
		removeTemporaryKnowledgeDisplay:function(){
			for(var i = 0; i < 6; ++i){
				gameInterface.updateKnowledge(i,private.knowledge[0][i],private.tempKnowledge[i]);
			}
			private.tempKnowledge = private.knowledge[0].slice(0);
			gameInterface.updateThoughts(private.thoughts[0]);
			private.tempThoughts = private.thoughts[0];
		},
		reset:function(){
			
			cards =[];
			page = 1; // might be a duplicate.
			selectedCardListCard = undefined;
			
			for(var i = 0; i < 25; ++i){
				document.getElementById("cardList"+i).style.visibility = "hidden";
				$("#cardList"+i).removeClass("selected").addClass("unselected");
			}
			document.getElementById("enemyNextTarget0").style.display = "block";
			document.getElementById("enemyNextTarget3").style.display = "block";
			document.getElementById("enemyNextTarget6").style.display = "block";
			document.getElementById("enemyNextTarget1").style.display = "none";
			document.getElementById("enemyNextTarget4").style.display = "none";
			document.getElementById("enemyNextTarget7").style.display = "none";
			document.getElementById("enemyNextTarget2").style.display = "none";
			document.getElementById("enemyNextTarget5").style.display = "none";
			document.getElementById("enemyNextTarget8").style.display = "none";
			
			document.getElementById("playerNextTarget0").style.display = "block";
			document.getElementById("playerNextTarget3").style.display = "block";
			document.getElementById("playerNextTarget6").style.display = "block";
			document.getElementById("playerNextTarget1").style.display = "none";
			document.getElementById("playerNextTarget4").style.display = "none";
			document.getElementById("playerNextTarget7").style.display = "none";
			document.getElementById("playerNextTarget2").style.display = "none";
			document.getElementById("playerNextTarget5").style.display = "none";
			document.getElementById("playerNextTarget8").style.display = "none";
			
			
			
			for(var i = 0; i < 5; ++i){
				document.getElementById("playerSoulFace"+i).style.display = "none";
				document.getElementById("playerSoulBack"+i).style.display = "none";
				
				document.getElementById("enemySoulFace"+i).style.display = "none";
				document.getElementById("enemySoulBack"+i).style.display = "none";
			}
			
			$(".LP1").each(function(index,element) {
				$(element).css("width","19.2%");
			});
			$(".LP2").each(function(index,element) {
				$(element).css("width","9.1%");
			});
			$(".LP3").each(function(index,element) {
				$(element).css("width","5.73333333333%");
			});
			
			
			for(var i = 0; i < 5; ++i){
				document.getElementById("enemyLP"+i+"-0-wrapper").style.display = "inline";
				for(var j = 1; j < 4; ++j){
					document.getElementById("enemyLP"+i+"-"+j+"-wrapper").style.display = "none";
				}
			}
			
			for(var i = 0; i < 5; ++i){
				document.getElementById("LP"+i+"-0-wrapper").style.display = "inline";
				for(var j = 1; j < 4; ++j){
					document.getElementById("LP"+i+"-"+j+"-wrapper").style.display = "none";
				}
			}
			
			for(var i = 0; i < 5; ++i){
				for(var j = 0; j < 2; ++j){
					gameInterface.restoreSoul(i,j);
				}
			}
			selectEnemyHand();
			
			
			
			
			
			gameInterface.updateThoughts(5); // this line not required
			gameInterface.updateEnemyThoughts(5);
			for(var i = 0; i < 10; ++i){
				for(var j = 0; j < 6; ++j){
					gameInterface.updateKnowledge(j,0,i);
					gameInterface.updateEnemyKnowledge(j,0,i);
				}
			}

			
			
			private.tempKnowledge = [0,0,0,0,0,0];
			private.tempThoughts = 5;
			private.knowledge = [[0,0,0,0,0,0],[0,0,0,0,0,0]];
			private.thoughts = [5,5];
			

			public.turnNumber = -1,
			public.cardsDrawn = [0,0],
			public.goFirst = !(public.goFirst),
			public.LP = [1,1],
			public.fieldData = [
			                    [new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData()],
			                    [new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData(),new NoCardData()]
			];
			public.setData = [new NoCardData(),new NoCardData()];
			public.soulData = [[],[]];
			public.handData = [[],[]];
			public.graveyardData = [[],[]];	
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			/// interface stuff
			
			gameInterface.reset();
			
			
			
			// HAVE NOT RESET FIELD DISPLAY YET. (INCLUDING MAKING SURE TO END THE TURN OF ANY CARDS (WHICH ALSO SHOULD BE DONE IF THEY ARE KILLED, ETC))
			// ALSO HAVE TO RESET THE BLUE BALLS.
		}
	};
	var private = {
		tempKnowledge:[0,0,0,0,0,0],
		tempThoughts:5,
		knowledge:[[0,0,0,0,0,0],[0,0,0,0,0,0]],
		thoughts:[5,5],
		reviveCard:function(index,player) {
			var c = public.fieldData[player][index];		
			c.dead = false; c.currentAttack = c.baseAttack; c.modifiedAttack =  c.baseAttack;
			c.currentDefense = c.baseDefense; c.modifiedDefense =  c.baseDefense;
			c.currentAgility = c.baseAgility; c.modifiedAgility =  c.baseAgility;
			c.currentRange = c.baseRange; c.modifiedRange =  c.baseRange;
			c.currentMana = c.baseMana; c.currentMaxMana =  c.baseMaxMana;
			c.currentLevel = c.baseLevel; c.modifiedLevel =  c.baseLevel; // MODIFIED CODE; was level not baseLevel.
			c.currentHP = c.baseMaxHP; c.currentMaxHP =  c.baseMaxHP; c.modifiedMaxHP = c.baseMaxHP;
			c.engaged = 0;
		}/*,
		setPlayerKnowledge:function(arg) {
			for(var i = 0; i < 6; ++i) {
				private.knowledge[0][i] = arg[i];
			}
			gameInterface.setPlayerKnowledge(private.knowledge[0]);
		},
		setEnemyKnowledge:function(arg) {
			for(var i = 0; i < 6; ++i) {
				private.knowledge[1][i] = arg[i];
				gameInterface.updateEnemyKnowledge(i);
			}
		}*/
	};
	return public;
})();
