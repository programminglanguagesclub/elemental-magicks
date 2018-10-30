var clockIntervalId = undefined;	
var clock = (function(){
	var public = {
		//playerTime:900,
		//enemyTime:900,
		playerTime:900000,
		enemyTime:900000,
		playerFlagsAt:-1,
		enemyFlagsAt:-1,
		playerTurn:false,
		updateClock:function() {
			if(public.playerFlagsAt === -1){
				public.playerFlagsAt = new Date().getTime() + public.playerTime;
			}
			public.playerTime = public.playerFlagsAt - new Date().getTime();
			var playerSeconds = Math.floor(public.playerTime/1000);
			if(gameData.gameStarted) {
				
				if(playerSeconds > 0){
					
					$("#playerClockDisplay").html(Math.floor(playerSeconds/60) + ":" + (playerSeconds%60 < 10 ? "0" : "") + playerSeconds%60);
				}
				else {
					$("#playerClockDisplay").html("0:00");
				}
			}
		},
		updateEnemyClock:function() {
			if(public.enemyFlagsAt === -1){
				public.enemyFlagsAt = new Date().getTime() + public.enemyTime;
			}
			public.enemyTime = public.enemyFlagsAt - new Date().getTime();
			var enemySeconds = Math.floor(public.enemyTime / 1000);
			if(gameData.gameStarted) {
				
				if(enemySeconds > 0) {
					
					$("#enemyClockDisplay").html(Math.floor(enemySeconds/60) + ":" + (enemySeconds%60 < 10 ? "0" : "") + enemySeconds%60);
				}
				else{
					$("#enemyClockDisplay").html("0:00");
				}
			} 
		},
		reset:function(){
			public.playerTime = 900000;
			public.enemyTime = 900000;
			playerSeconds = Math.floor(public.playerTime/1000);
			enemySeconds = Math.floor(public.enemyTime / 1000);
			$("#playerClockDisplay").html(Math.floor(playerSeconds/60) + ":" + (playerSeconds%60 < 10 ? "0" : "") + playerSeconds%60);
			$("#enemyClockDisplay").html(Math.floor(enemySeconds/60) + ":" + (enemySeconds%60 < 10 ? "0" : "") + enemySeconds%60);
		}
	};
	return public;
})();
