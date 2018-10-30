var renderCardStats = (function(){
	var privateScope = {
		
	};
	var publicScope = {
		hp:function(current, permanent, base){
			var retVal = "";
			if(current === permanent){
				retVal += current;
			}
			else{
				retVal += current + "/" + permanent;
			}
			if(permanent !== base){
				retVal += " (" + base + ")";
			}
			return retVal;
		},
		mana:function(current,permanent,base){
			return publicScope.hp(current,permanent,base);
		},
		attack:function(current, permanent, base){
			return publicScope.hp(current,permanent,base);
		},
		defense:function(current, permanent, base){
			return publicScope.hp(current,permanent,base);
		},
		agility:function(current, permanent, base){
			return publicScope.hp(current,permanent,base);
		},
		range:function(current, permanent, base){
			return publicScope.hp(current,permanent,base);
		},
		engagement:function(current){
			return current;
		}
	};
	return publicScope;
}());


