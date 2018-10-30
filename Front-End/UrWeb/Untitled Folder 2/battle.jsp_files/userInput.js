var userInput =
{
	deployCard:function(arg) { post({"type":"DeployCard","location":arg}); },
	// for these cardList functions don't check for reaching the end of the card list yet.
	cardListRight: function() {cardList.cardListRight(); },
	cardListLeft: function() {cardList.cardListLeft();},
	
	skip: function() {
		//post([{"type":"Skip","schoolLevel":gameData.tempKnowledge}]);
		
		post({"type":"Skip","schoolLevel":gameData.getAllTempKnowledge()}); 		
		
		
		
	},
	resignRound:function() {
		post({"type":"ResignRound"});
	},
	resignGame:function(){
		post({"type":"ResignGame"});
	},
	directAttack:function() { post({"type":"DirectAttack"}); },
	rest:function() { post({"type":"Rest"}); },
	setCard: function() {		
		// for now, let us try to set the last card in the hand, not any particular card.
		
		
		
		
		//post([{"type":"SetCard","schoolLevel":[gameData.tempKnowledge[0],gameData.tempKnowledge[1],gameData.tempKnowledge[2],gameData.tempKnowledge[3],gameData.tempKnowledge[4],gameData.tempKnowledge[5]],"card":phase.selectedPositions[2][0]}]); 		
		
		
		post({"type":"SetCard","schoolLevel":gameData.getAllTempKnowledge(),"card":phase.selectedPositions[2][0]}); 		
		
		
		
		// just sending "legal", and having the client know what to do is an optimization that I may work on later.	
	},
	// the use of id here only works for the first page.... MUST CHANGE LATER!!!!!!!!!
	drawCard: function() { post({"type":"DrawCard","soulIndex":gameInterface.getSoulIndex(),"id":gameInterface.selectedCardId+1}); },
	incrementKnowledge:function(index) // this deals with direct attempts to adjust the temp value by the user
	{
		if(gameData.tempThoughtCount > 0){
			if(gameData.tempKnowledge[index] > 8) // don't allow user to train beyond level 9
				alert("You have already mastered all attainable knowledge on this subject");
			else {
				--gameData.tempThoughtCount;
				++gameData.tempKnowledge[index];
				gameInterface.updateKnowledge(index);
				gameInterface.updateThoughts();
			} }
		else alert("Insufficient thoughts. Your mental activity cannot tolerate further training."); },
	decrementKnowledge:function(index) // this deals with direct attempts to adjust the temp value by the user
	{
		if(gameData.tempKnowledge[index] > gameData.knowledge[index]) {
			--gameData.tempKnowledge[index]; ++gameData.tempThoughtCount;
			gameInterface.updateKnowledge(index); gameInterface.updateThoughts(); }
		else alert("You may not lower your education"); },
	confirmRevive:function() { post({"type":"ReviveCards","revivedCards":[phase.selectedPositions[0].contains(0),
	    phase.selectedPositions[0].contains(1),phase.selectedPositions[0].contains(2),phase.selectedPositions[0].contains(3),phase.selectedPositions[0].contains(4),
	    phase.selectedPositions[0].contains(5),phase.selectedPositions[0].contains(6),phase.selectedPositions[0].contains(7),phase.selectedPositions[0].contains(8),]}); },
	// phase.name should be able to work....
	confirmDeploy:function() { 
		if(phase.getName() == "place") {
			if(phase.selectedPositions[0].length === 1) {
				post({"type":"DeployCard","location":phase.selectedPositions[0][0]});
			}
		//gameController.placeCard(phase.placePhase.selectedPosition);
		}
	},
	selectRow:function(arg) {
		document.getElementById('row0').style.visibility = 'hidden'; document.getElementById('row1').style.visibility = 'hidden'; document.getElementById('row2').style.visibility = 'hidden';
		post({"type":"Attack","row":arg}); },
	initiateActionSkill:function(arg) {
		post({type:"InitiateActionSkill",skillIndex:arg});
	},
	returnPressed:function() { //	chatMessagePost(); // no check for empty string currently.
	},
	requestCards:function(page) { // BROKEN
		/*
		document.getElementById("drawCardButton").style.visibility = 'visible'; document.getElementById("handButton").style.visibility = 'visible';
		document.getElementById("browseCardsButton").style.visibility = 'hidden'; post([{"type":"RequestPartialCardList","page":page}]);
		*/
	}
};
