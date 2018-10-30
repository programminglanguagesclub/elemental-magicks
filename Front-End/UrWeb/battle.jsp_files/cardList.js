var cardList = (function(){
	var private = {
		//PAGINATION_SIZE:9,
		
		numberOfCards:9, // have to update this value immediately.
		currentPage:0, // 0-indexed	
		drawCard:function(id){
			
		},
		queryCardList:function(/*minLevel,maxLevel,requiredSchools,requireUnit,requireSpell,*/){
			// post to server with currentPage, other values that are retrieved from GUI here.
			// update cardIds, cardsInCurrentSearch (essentially the server says how many cards there are (cardsInCurrentSearch), but only returns some of them (ided by cardIds, but also other information will be provided which will populate the interface here.)
		
			post([{"type":"RequestPartialCardList",page:private.currentPage}]); // not posting other filters currently.
			
			//I SHOULD NOT CALL MY POST HELPER HERE BUT INSTEAD USE JQUERY DIRECTLY TO GET AROUND MY FRAMEWORK.		
// I MIGHT HAVE TO DO SOMETHING TO GET AROUND THE PAYLOAD SYSTEM IN ORDER TO NOT BREAK THINGS WITH THIS THOUGH (the payload system may be useful for replays and reconnecting, but it seriously needs some work)


/*
			$.post(servlet,{json:JSON.stringify(arg),channelToken:sessionStorage.token,channelKey:sessionStorage.userId} ,function(payload){
				var data = $.parseJSON(payload.data);
				private.cardIds = data.cardIds;
				private.cardsInCurrentSearch = data.cardsInCurrentSearch;
				private.updateInterface(data.cards);				
			});
			*/
		},
		updateInterface:function(cards){
			
		}
	};
	var public = {
		cardIds:[1,2,3,4,5,6,7,8,9],
		drawCard:function(position){
			private.drawCard(private.cardIds[position]);		
		},
	cardListLeft:function(){
			--private.currentPage;
			private.queryCardList();
		},
		cardListRight:function(){
			++private.currentPage;
			private.queryCardList();
		},
		initialQuery:function(){
			queryCardList();
			// disable interface directly from here as (if there are < 10 cards in game) I may have to disable cardListRight AND cardListLeft.
		}	
	};
	return public;
}());