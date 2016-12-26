var canvas = document.getElementById("c");
var renderer = new THREE.WebGLRenderer({canvas: canvas});
var camera = new THREE.PerspectiveCamera( 20, 1, 1, 10000 );
var scene = new THREE.Scene();
var sphereGeo = new THREE.PlaneGeometry( 90, 110, 1, 1 );
camera.position.z = 800;
THREE.ImageUtils.crossOrigin = ''; // Ended up not using this yet. Will want it when I start using actual images files.
var cardFrontTexture = new THREE.Texture(cardFrontImage); // only the abomination, for now.
var cardBackTexture = new THREE.Texture(cardBackImage);
cardFrontTexture.needsUpdate = true;
cardBackTexture.needsUpdate = true;
//-----------------------------------------------------------------------
var cardMeshes = [];
var selectionMeshes = [];
var selectedField = [false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false];
//-----------------------------------------------------------------------
function getCardFront(){
  var cardFrontTexture = new THREE.Texture(cardFrontImage);
  cardFrontTexture.needsUpdate = true;
  var cardFront = new THREE.Mesh(
    new THREE.PlaneGeometry(20,25),
    new THREE.MeshPhongMaterial({ map: cardFrontTexture, shininess: 50 })
  );
  cardFront.material.transparent = true;
  cardMeshes.push(cardFront);
  return cardFront;
}
//-----------------------------------------------------------------------
function getCardBack(){
  var cardBackTexture = new THREE.Texture(cardBackImage);
  cardBackTexture.needsUpdate = true;
  var backGeometry = new THREE.PlaneGeometry(20,25);
  backGeometry.applyMatrix( new THREE.Matrix4().makeRotationY( Math.PI ) );
  var cardBack = new THREE.Mesh(
    backGeometry,
    new THREE.MeshPhongMaterial({ map: cardBackTexture, shininess: 1 })
  );
  cardBack.position.set(0,0,.0001);
  cardBack.material.transparent = true;
  return cardBack;
}
//-----------------------------------------------------------------------
function getCard(){
  var card = new THREE.Object3D();
  card.add(getCardFront());
  card.add(getCardBack());
  return card;
}
//-----------------------------------------------------------------------
function getCardSelection(){
  var cardSlot = new THREE.Mesh(
  new THREE.PlaneGeometry(22,27),
  new THREE.MeshBasicMaterial({color:0x888888}))
  selectionMeshes.push(cardSlot);
  return cardSlot;
}
//-----------------------------------------------------------------------
var positionImages = [position1, position2, position3, position4, position5, position6, position7, position8, position9];
var fieldPositions = [];
var cards = [];
var cardSlots = [];
//-----------------------------------------------------------------------
function getFieldIndex(index){
  var positionTexture = new THREE.Texture(positionImages[index]);
  positionTexture.needsUpdate = true;
  return new THREE.Mesh(
  new THREE.PlaneGeometry(19,23.75),
  new THREE.MeshPhongMaterial({ map: positionTexture, shininess: 1 }))
}
//-----------------------------------------------------------------------
function getCardSlot(index){
  var cardSlot = new THREE.Object3D();
  var card = getCard();
  var cardSelection = getCardSelection();
  var fieldIndex = getFieldIndex(index);
  cardSlot.add(card);
  cardSlot.add(cardSelection);
  cardSlot.add(fieldIndex);
  cardSelection.position.set(0,0,-1);
  fieldIndex.position.set(0,0,-.5);
  fieldPositions.push(fieldIndex);
  cardSlots.push(cardSelection);
  cards.push(card);
  return cardSlot;
}
//------------------------------------------------------------------------
function getSpawn(){
  var cardSlot = new THREE.Object3D();
  var card = getCard();
  var cardSelection = getCardSelection();
  var fieldIndex = getFieldIndex(4); // ALMOST SAME CODE AS GET FIELD INDEX....
  cardSlot.add(card);
  cardSlot.add(cardSelection);
  cardSlot.add(fieldIndex);
  cardSelection.position.set(0,0,-1);
  fieldIndex.position.set(0,0,-.5);
  fieldPositions.push(fieldIndex);
  cardSlots.push(cardSelection);
  cards.push(card);
  return cardSlot;
}
//-----------------------------------------------------------------------

function getBoard(player){
  var woodTexture = new THREE.Texture(wood);
  woodTexture.needsUpdate = true;
  var boardContainer = new THREE.Object3D();
  var board = new THREE.Mesh(
    new THREE.PlaneGeometry(90,80),
    new THREE.MeshPhongMaterial({ map: woodTexture, shininess: 0, transparent : true })
  )
  board.position.set(0,0,0);
  playerBoard = board;
  boardContainer.add(board);
  board.rotation.z = Math.PI/2; 
  for(var j = 0; j < 3; ++j){
    for(var i = 0; i < 3; ++i){
      var cardSlot = getCardSlot(j*3 + i);
      cardSlot.position.set(25 * i - 25, 30 * (2-j) - 30, 2);
      boardContainer.add(cardSlot);
    }
  }
  if(player){
    var spawn = getSpawn();
    spawn.position.set(0,-60,2);
    boardContainer.add(spawn);
  }
  else{
     var spawn = getSpawn();
    spawn.position.set(0,60,2);
    boardContainer.add(spawn);
  }
  return boardContainer;
}
//------------------------------------------------------------------------
function getSoul(soulIndex){
  var cardSlot = new THREE.Object3D();
  var card = getCard();
  var cardSelection = getCardSelection();
  var fieldIndex = getFieldIndex(4); // ALMOST SAME CODE AS GET FIELD INDEX....
  cardSlot.add(card);
  cardSlot.add(cardSelection);
  cardSlot.add(fieldIndex);
  cardSelection.position.set(0,0,-1);
  fieldIndex.position.set(0,0,-.5);
  fieldPositions.push(fieldIndex);
  cardSlots.push(cardSelection);
  cards.push(card);
  return cardSlot;
}
//-----------------------------------------------------------------------
function getSouls(player){
  var woodTexture = new THREE.Texture(wood); // might want a different texture for this one eventually.
  woodTexture.needsUpdate = true;
  var boardContainer = new THREE.Object3D();
  var board = new THREE.Mesh(
    new THREE.PlaneGeometry(30,130),
    new THREE.MeshPhongMaterial({ map: woodTexture, shininess: 0, transparent : true })
  )
  board.position.set(0,0,0);
  playerBoard = board;
  boardContainer.add(board);
  board.rotation.z = Math.PI/2; 
  for(var i = 0; i < 5; ++i){
    var cardSlot = getSoul(i);
    cardSlot.position.set(25 * i - 50, 0, 2);
    boardContainer.add(cardSlot);
  }
  return boardContainer;
}
//-------------------------------------------------------------------------




// bunch of variables


var soulContainer = getSouls(true);
soulContainer.position.set(-40,-10,98);
scene.add(soulContainer);

var opponentsoulContainer = getSouls(false);
opponentsoulContainer.position.set(-40,47,98);
scene.add(opponentsoulContainer);




var boardContainer = getBoard(true);
boardContainer.position.set(-205,-48,98);
scene.add(boardContainer);

var opponentBoardContainer = getBoard(false);
opponentBoardContainer.position.set(-205,47,98);
scene.add(opponentBoardContainer);

var orbLevel = [0,0,0,0,0,0];
var orbSpeed = [1/3, 1/3, 1/3, 1/3, 1/3, 1/3];
var beginFade = [2000,1000,0,2000,2000,0000];
var endFade = [7000,7000,3000,7000,7000,5000];
var numParticles = [60,60,30,20,60,60];
var orbCoordinates = [[-130,-75,120],[-100,-75,120],[-70,-75,120],[-40,-75,120],[-10,-75,120],[20,-75,120]];

var fireTransitions = [];
for(var i = 0; i < 22; ++i){
 fireTransitions[i] = (1000 + (100 * i)); 
  //hardcoded in update as well
}

var smokeGeo = new THREE.PlaneGeometry(30,30);
var earthParticles = [];
var flameParticles = [];
var waterParticles = [];
var windParticles = [];
var spiritParticles = [];
var voidParticles = [];

//--------------------------------------------------------------------------
var ambientLight = new THREE.AmbientLight( 0xCCCCCC );
scene.add(ambientLight);
var directionalLight = new THREE.DirectionalLight( 0xcccccc, 0.5 );
directionalLight.position.set( 0, 1, 2000 );
scene.add( directionalLight );
//--------------------------------------------------------------------------

// set up textures for flame effect

var flames2Texture = new THREE.Texture(flames2Image);
flames2Texture.needsUpdate = true;

var flames3Texture = new THREE.Texture(flames3Image);
flames3Texture.needsUpdate = true;

var flames4Texture = new THREE.Texture(flames4Image);
flames4Texture.needsUpdate = true;

var flames5Texture = new THREE.Texture(flames5Image);
flames5Texture.needsUpdate = true;

var flames6Texture = new THREE.Texture(flames6Image);
flames6Texture.needsUpdate = true;

var flames7Texture = new THREE.Texture(flames7Image);
flames7Texture.needsUpdate = true;

var flames8Texture = new THREE.Texture(flames8Image);
flames8Texture.needsUpdate = true;

var flames9Texture = new THREE.Texture(flames9Image);
flames9Texture.needsUpdate = true;

var flames10Texture = new THREE.Texture(flames10Image);
flames10Texture.needsUpdate = true;

var flames11Texture = new THREE.Texture(flames11Image);
flames11Texture.needsUpdate = true;

var flames12Texture = new THREE.Texture(flames12Image);
flames12Texture.needsUpdate = true;

var flames13Texture = new THREE.Texture(flames13Image);
flames13Texture.needsUpdate = true;

var flames14Texture = new THREE.Texture(flames14Image);
flames14Texture.needsUpdate = true;

var flames15Texture = new THREE.Texture(flames15Image);
flames15Texture.needsUpdate = true;

var flames16Texture = new THREE.Texture(flames16Image);
flames16Texture.needsUpdate = true;

var flames17Texture = new THREE.Texture(flames17Image);
flames17Texture.needsUpdate = true;

var flames18Texture = new THREE.Texture(flames18Image);
flames18Texture.needsUpdate = true;

var flames19Texture = new THREE.Texture(flames19Image);
flames19Texture.needsUpdate = true;

var flames20Texture = new THREE.Texture(flames20Image);
flames20Texture.needsUpdate = true;

var flames21Texture = new THREE.Texture(flames21Image);
flames21Texture.needsUpdate = true;

var flames22Texture = new THREE.Texture(flames22Image);
flames22Texture.needsUpdate = true;

var flames23Texture = new THREE.Texture(flames23Image);
flames23Texture.needsUpdate = true;
//----------------------------------------------------------------------------

var plusButtons = [];
var downButtons = [];
var plusTexture = new THREE.Texture(plusImage);
plusTexture.needsUpdate = true;
var minusTexture = new THREE.Texture(minusImage);
minusTexture.needsUpdate = true;

for(var i = 0; i < 6; ++i){
  var upArrow = new THREE.Mesh(
    new THREE.PlaneGeometry(10,10),
    new THREE.MeshPhongMaterial({color: 0xffffff, map : plusTexture,  blending: THREE.AdditiveBlending})
  );
  upArrow.position.set(-130 + (i * 30), -50, 120);
  plusButtons.push(upArrow);
  scene.add(upArrow);
  var downArrow = new THREE.Mesh(
    new THREE.PlaneGeometry(10,10),
    new THREE.MeshBasicMaterial({ color: 0xffffff, map : minusTexture,  blending: THREE.AdditiveBlending })
  );
  downArrow.position.set(-130 + (i * 30), -100, 120);
  downButtons.push(downArrow);
  scene.add(downArrow);
}
/*
var orbBackground = new THREE.Mesh(
  new THREE.PlaneGeometry(350,100),
  new THREE.MeshBasicMaterial({ color: 0x444444 })
);
orbBackground.position.set(0, -80, 29);
scene.add(orbBackground);
*/

var orbBackground = new THREE.Mesh(
  new THREE.PlaneGeometry(2000,2000),
  new THREE.MeshBasicMaterial({ color: 0x444444 })
);
orbBackground.position.set(0, 0, 0);
scene.add(orbBackground);

var clearTexture = new THREE.Texture(clearImage);
clearTexture.needsUpdate = true;
var positionTextures = [clearTexture];
for(var i = 0; i < 9; ++i){
  positionTextures[i+1] = new THREE.Texture(positionImages[i]);
  positionTextures[i+1].needsUpdate = true;
}





// MORE WORK

// probably need to make the text on the original image bigger so that this can scale up without getting pixelated.

//used a cursive font that starts with a U in gimp

var knowledgeTexture = new THREE.Texture(knowledgeImage);
knowledgeTexture.needsUpdate = true;

var knowledgeGeo = new THREE.Mesh(
  new THREE.PlaneGeometry(50,12.5),
  new THREE.MeshBasicMaterial({ color: 0xffffff, map : knowledgeTexture,  blending: THREE.AdditiveBlending, transparent: true })
);
/*
var knowledgeGeo = new THREE.SphereGeometry(10, 10, 10);// args are radius, and then segments in x and y.
var knowledgeMat = new THREE.MeshPhongMaterial(  { ambient: 0x555555, color: 0x006600, specular: 0x222222, shininess: 10 , map :  knowledgeTexture});
var knowledgeSphere = new THREE.Mesh(knowledgeGeo, earthMat);
*/

knowledgeGeo.position.set(-55, -36, 120);
knowledgeGeo.needsUpdate = true;
scene.add(knowledgeGeo);





var thoughtsTexture = new THREE.Texture(thoughtsImage);
thoughtsTexture.needsUpdate = true;

var thoughtsGeo = new THREE.Mesh(
  new THREE.PlaneGeometry(50,12.5),
  new THREE.MeshBasicMaterial({ color: 0xffffff, map : thoughtsTexture,  blending: THREE.AdditiveBlending, transparent: true })
);


thoughtsGeo.position.set(60, -36, 120);
thoughtsGeo.needsUpdate = true;
scene.add(thoughtsGeo);









// Eventually I need a whole system for thoughts. For now just this.





var numberImages = [number0Image, number1Image, number2Image, number3Image, number4Image, number5Image, number6Image, number7Image, number8Image, number9Image];
var numberTextures = [];
for(var i = 0; i < 10; ++i){
 numberTextures[i] = new THREE.Texture(numberImages[i]);
 numberTextures[i].needsUpdate = true;
}



var number0Texture = numberTextures[0];

var number0Geo = new THREE.Mesh(
  new THREE.PlaneGeometry(15,15),
  new THREE.MeshBasicMaterial({ color: 0xffffff, map : number0Texture,  blending: THREE.AdditiveBlending, transparent: false })
);


number0Geo.position.set(40, -75, 120);
number0Geo.needsUpdate = true;
scene.add(number0Geo);


var number1Geo = new THREE.Mesh(
  new THREE.PlaneGeometry(15,15),
  new THREE.MeshBasicMaterial({ color: 0xffffff, map : number0Texture,  blending: THREE.AdditiveBlending, transparent: false })
);


number1Geo.position.set(57, -75, 120);
number1Geo.needsUpdate = true;
scene.add(number1Geo);



var number2Geo = new THREE.Mesh(
  new THREE.PlaneGeometry(15,15),
  new THREE.MeshBasicMaterial({ color: 0xffffff, map : number0Texture,  blending: THREE.AdditiveBlending, transparent: false })
);


number2Geo.position.set(74, -75, 120);
number2Geo.needsUpdate = true;
scene.add(number2Geo);











var hpGeometry = new THREE.CylinderGeometry( 15, 15, 15, 50, 1, false, 0, Math.PI/2 );
hpGeometry.applyMatrix( new THREE.Matrix4().makeRotationZ( Math.PI/2 ) );
//hpGeometry.applyMatrix( new THREE.Matrix4().makeRotationZ( Math.PI/4 ) );
//hpGeometry.applyMatrix( new THREE.Matrix4().makeRotationY( Math.PI/8 ) );
var material = new THREE.MeshPhongMaterial( { ambient: 0xaa2200, color: 0xaa000, specular: 0x555500, shininess: 4 } );
var cylinder = new THREE.Mesh( hpGeometry, material );



scene.add( cylinder );



var hpGeometry2 = new THREE.CylinderGeometry( 15, 15, 15, 50, 1, false, 0, Math.PI/2 );
hpGeometry2.applyMatrix( new THREE.Matrix4().makeRotationZ( Math.PI/2 ) );
var material2 = new THREE.MeshPhongMaterial( { ambient: 0xaa2200, color: 0xaa000, specular: 0x555500, shininess: 4 } );
var cylinder2 = new THREE.Mesh( hpGeometry2, material2 );


cylinder2.position.set(16,0,0);
scene.add( cylinder2 );

// END MORE WORK


// I Probably want to move the board to the middle of the interface, and move everything else to the left.
// That way the main action that the player is looking at will be in the center of the screen.



//-------------------------------------------------------------------------------------------------------------
var earthGeo = new THREE.SphereGeometry(10, 10, 10);// args are radius, and then segments in x and y.
var earthMat = new THREE.MeshPhongMaterial(  { ambient: 0x555555, color: 0x006600, specular: 0x222222, shininess: 10 , map :  positionTextures[0]});
var earthSphere = new THREE.Mesh(earthGeo, earthMat);
earthGeo.applyMatrix( new THREE.Matrix4().makeRotationY( 3 * Math.PI/2 ) ); // so that text shows up in the correct position.
scene.add(earthSphere);
earthSphere.position.set(orbCoordinates[0][0], orbCoordinates[0][1], orbCoordinates[0][2]);
var fireGeo = new THREE.SphereGeometry(10, 10, 10);
var fireMat = new THREE.MeshPhongMaterial( { ambient: 0x555555, color: 0xff0000, specular: 0x222222, shininess: 10 , map : positionTextures[0]} );
var fireSphere = new THREE.Mesh(fireGeo, fireMat);
fireGeo.applyMatrix( new THREE.Matrix4().makeRotationY( 3 * Math.PI/2 ) );
scene.add(fireSphere);
fireSphere.position.set(orbCoordinates[1][0], orbCoordinates[1][1], orbCoordinates[1][2]);
var waterGeo = new THREE.SphereGeometry(10, 10, 10);
var waterMat = new THREE.MeshPhongMaterial( { ambient: 0x555555, color: 0x8888ff, specular: 0x222222, shininess: 10, map : positionTextures[0] } );
var waterSphere = new THREE.Mesh(waterGeo, waterMat);
waterGeo.applyMatrix( new THREE.Matrix4().makeRotationY( 3 * Math.PI/2 ) );
scene.add(waterSphere);
waterSphere.position.set(orbCoordinates[2][0], orbCoordinates[2][1], orbCoordinates[2][2]);
var airGeo = new THREE.SphereGeometry(10, 10, 10);
var airMat = new THREE.MeshPhongMaterial( { ambient: 0x555555, color: 0xffffff, specular: 0x222222, shininess: 10 , map : positionTextures[0]} );
var airSphere = new THREE.Mesh(airGeo, airMat);
airGeo.applyMatrix( new THREE.Matrix4().makeRotationY( 3 * Math.PI/2 ) );
scene.add(airSphere);
airSphere.position.set(orbCoordinates[3][0], orbCoordinates[3][1], orbCoordinates[3][2]);
var spiritGeo = new THREE.SphereGeometry(10, 10, 10);
var spiritMat = new THREE.MeshPhongMaterial( { ambient: 0x555555, color: 0x440088, specular: 0x222222, shininess: 10 , map : positionTextures[0]} );
var spiritSphere = new THREE.Mesh(spiritGeo, spiritMat);
spiritGeo.applyMatrix( new THREE.Matrix4().makeRotationY( 3 * Math.PI/2 ) );
scene.add(spiritSphere);
spiritSphere.position.set(orbCoordinates[4][0], orbCoordinates[4][1], orbCoordinates[4][2]);
var voidGeo = new THREE.SphereGeometry(10, 10, 10);
var voidMat = new THREE.MeshPhongMaterial( { ambient: 0x555555, color: 0x222222, specular: 0x111111, shininess: 10 , map : positionTextures[0]} );
var voidSphere = new THREE.Mesh(voidGeo, voidMat);
voidGeo.applyMatrix( new THREE.Matrix4().makeRotationY( 3 * Math.PI/2 ) );
scene.add(voidSphere);
voidSphere.position.set(orbCoordinates[5][0], orbCoordinates[5][1], orbCoordinates[5][2]);
var orbMats = [earthMat, fireMat, waterMat, airMat, spiritMat, voidMat];










// NEW WORK

/*
var thoughtGeo = new THREE.SphereGeometry(20, 20, 20);// args are radius, and then segments in x and y.
var thoughtMat = new THREE.MeshPhongMaterial(  { ambient: 0x555555, color: 0x999999, specular: 0x222222, shininess: 10 , map :  positionTextures[5]});
var thoughtSphere = new THREE.Mesh(thoughtGeo, thoughtMat);
thoughtGeo.applyMatrix( new THREE.Matrix4().makeRotationY( 3 * Math.PI/2 ) ); // so that text shows up in the correct position.



thoughtSphere.position.set(100, -75, 120);

scene.add(thoughtSphere);
*/



// END NEW WORK















//------------------------------------------------------------------------------------------------------------------
function initParticle(particle, thisLastUpdate, orb, id){ // id only useful for flame currently.
  particle.position.set(orbCoordinates[orb][0], orbCoordinates[orb][1], orbCoordinates[orb][2]);
  particle.material.opacity = 0;
  particle.material.needsUpdate = true;
  var angle = Math.random() * 2 * Math.PI;
  return {particle : particle, xVelocity : Math.cos(angle) , yVelocity : Math.sin(angle), timeStep : 0, lastUpdate : thisLastUpdate, id : id};
}
function initWaterParticle(particle, thisLastUpdate, orb, id){ // id only useful for flame currently.
  particle.position.set(orbCoordinates[orb][0], orbCoordinates[orb][1], orbCoordinates[orb][2]);
  particle.material.opacity = 0;
  particle.material.needsUpdate = true;
  return {particle : particle, xVelocity : 0 , yVelocity : -1, timeStep : 0, lastUpdate : thisLastUpdate, id : id};
}
//-----------------------------------------------------------------------------------------
function updateFlameParticle(particle, currentTime){
  var orb = 1;
  if(currentTime < particle.lastUpdate){
    return;
  }
  var timeStep = currentTime - particle.lastUpdate;
  particle.timeStep += timeStep;
  if(particle.timeStep >= fireTransitions[0]){
    particle.particle.position.x += (particle.xVelocity * orbLevel[orb] * orbSpeed[orb] * timeStep / 500);
    particle.particle.position.y += (particle.yVelocity * orbLevel[orb] * orbSpeed[orb] * timeStep / 500);
    particle.particle.material.opacity = (endFade[orb] - particle.timeStep) / (endFade[orb] - beginFade[orb]);
    if(particle.particle.material.opacity < 0){
      particle.particle.material.opacity = 0;
    }
    var matIndex = Math.floor((particle.timeStep - 1000) / 100);
    if(matIndex > 21){
      matIndex = 21; // this can be due to lag, switching tabs, etc.
    }
    // handle transition to new material
    if(particle.particle.material === flameMaterials[matIndex][particle.id]){

    }
    else{
      particle.particle.material = flameMaterials[matIndex][particle.id];
    }
    if(particle.timeStep >= fireTransitions[21]){
      resetFlameParticle(particle, currentTime + (particle.timeStep - endFade[orb]), orb);
      particle.particle.material.opacity = 1;
    }
    particle.particle.material.needsUpdate = true;
  }
  if(orbLevel[1] === 0){
    particle.particle.material.opacity = 0;
  }
  particle.lastUpdate = currentTime;
}
function updateWaterParticle(particle, currentTime, orb){
  if(currentTime < particle.lastUpdate){ return; }
  var timeStep = currentTime - particle.lastUpdate;
  particle.timeStep += timeStep;
  if(particle.timeStep >= beginFade[orb]){
    particle.particle.position.x += (particle.xVelocity * orbLevel[orb] * orbSpeed[orb] * timeStep / 500);
    particle.particle.position.y += (particle.yVelocity * orbLevel[orb] * orbSpeed[orb] * timeStep / 500);
    particle.particle.material.opacity = (endFade[orb] - particle.timeStep) / (endFade[orb] - beginFade[orb]);
    if(particle.particle.material.opacity < 0){
      particle.particle.material.opacity = 0;
    }
    if(particle.timeStep >= endFade[orb]){
      resetWaterParticle(particle, currentTime + (particle.timeStep - endFade[orb]), orb);
      particle.particle.material.opacity = 1;
    }
    particle.particle.material.needsUpdate = true;
  }
  if(orbLevel[orb] === 0){
    particle.particle.material.opacity = 0;
  }
  particle.lastUpdate = currentTime;
}
function updateParticle(particle, currentTime, orb){
  if(currentTime < particle.lastUpdate){
    return;
  }
  var timeStep = currentTime - particle.lastUpdate;
  particle.timeStep += timeStep;
  if(particle.timeStep >= beginFade[orb]){
    particle.particle.position.x += (particle.xVelocity * orbLevel[orb] * orbSpeed[orb] * timeStep / 500);
    particle.particle.position.y += (particle.yVelocity * orbLevel[orb] * orbSpeed[orb] * timeStep / 500);
    particle.particle.material.opacity = (endFade[orb] - particle.timeStep) / (endFade[orb] - beginFade[orb]);
    if(particle.particle.material.opacity < 0){
      particle.particle.material.opacity = 0;
    }
    if(particle.timeStep >= endFade[orb]){
      resetParticle(particle, currentTime + (particle.timeStep - endFade[orb]), orb);
      particle.particle.material.opacity = 1;
    }
    particle.particle.material.needsUpdate = true;
  }
  if(orbLevel[orb] === 0){
    particle.particle.material.opacity = 0;
  }
  particle.lastUpdate = currentTime;
}
//----------------------------------------------------------------------------------------------------------------------------------------
function resetParticle(particleRecord, thisLastUpdate, orb){
  particleRecord.particle.position.set(orbCoordinates[orb][0], orbCoordinates[orb][1], orbCoordinates[orb][2]);
  particleRecord.particle.material.needsUpdate = true;
  var angle = Math.random() * 2 * Math.PI;
  particleRecord.xVelocity = Math.cos(angle);
  particleRecord.yVelocity = Math.sin(angle);
  particleRecord.timeStep = 0;
  particleRecord.lastUpdate = thisLastUpdate;
  return particleRecord;
}
function resetWaterParticle(particleRecord, thisLastUpdate, orb){
  particleRecord.particle.position.set(orbCoordinates[orb][0], orbCoordinates[orb][1], orbCoordinates[orb][2]);
  particleRecord.particle.material.needsUpdate = true;
  particleRecord.xVelocity = 0;
  particleRecord.yVelocity = -1;
  particleRecord.timeStep = 0;
  particleRecord.lastUpdate = thisLastUpdate;
  return particleRecord;
}
function resetFlameParticle(particleRecord, thisLastUpdate){
  var orb = 1;
  particleRecord.particle.position.set(orbCoordinates[orb][0], orbCoordinates[orb][1], orbCoordinates[orb][2]);
  particleRecord.particle.material = flameMaterials[0][particleRecord.id];
  particleRecord.particle.material.needsUpdate = true;
  var angle = Math.random() * 2 * Math.PI;
  particleRecord.xVelocity = Math.cos(angle);
  particleRecord.yVelocity = Math.sin(angle);
  particleRecord.timeStep = 0;
  particleRecord.lastUpdate = thisLastUpdate;
  return particleRecord;
}
//----------------------------------------------------------------------------------------------------------------------------------------------
smokeTexture = new THREE.Texture(smokeImage);
smokeTexture.needsUpdate = true;
waterTexture = new THREE.Texture(waterImage);
waterTexture.needsUpdate = true;
var flameTextures = [
 flames2Texture,
 flames3Texture,
 flames4Texture,
 flames5Texture,
 flames6Texture,
 flames7Texture,
 flames8Texture,
 flames9Texture,
 flames10Texture,
 flames11Texture,
 flames12Texture,
 flames13Texture,
 flames14Texture,
 flames15Texture,
 flames16Texture,
 flames17Texture,
 flames18Texture,
 flames19Texture,
 flames20Texture,
 flames21Texture,
 flames22Texture,
 flames23Texture
];
var flameMaterials = [];
for(var i = 0; i < 22; ++i){
  flameMaterials[i] = [];
  for(var j = 0; j < numParticles[1]; ++j){
    flameMaterials[i][j] = new THREE.MeshPhongMaterial({color: 0x000000, map: flameTextures[i], blending: THREE.AdditiveBlending, transparent: true});
  }
}
var airParticleMaterial = new THREE.MeshPhongMaterial({color: 0x000000, map: smokeTexture, transparent: true, emissive : 0x000000});
//------------------------------------------------------------------------------------------------------------------------------------------------
var currentTime = new Date().getTime();
//------------------------------------------------------------------------------------------------------------------------------------------------
for (var p = 0; p < numParticles[0]; p++) {
  var earthParticleMaterial = new THREE.MeshPhongMaterial({color: 0x005500, map: smokeTexture, transparent: true, emissive : 0x005500, emissiveIntensity : 0.0, specular : 0x000000});
  var particle = new THREE.Mesh(smokeGeo,earthParticleMaterial);
  particle.material.transparent = true;
  particle.material.opacity = 1;
  particle.position.set(orbCoordinates[0][0], orbCoordinates[0][1], orbCoordinates[0][2]);
  particle.rotation.z = Math.random() * 360;
  scene.add(particle);
  earthParticles.push(initParticle(particle, currentTime + (endFade[0] / numParticles[0]) * p, 0));
}
for (p = 0; p < numParticles[1]; p++) {
  var flameParticleMaterial = flameMaterials[6][p];
  var particle = new THREE.Mesh(smokeGeo,flameParticleMaterial);
  particle.material.opacity = 0;
  particle.material.needsUpdate = true;
  particle.position.set(orbCoordinates[1][0], orbCoordinates[1][1], orbCoordinates[1][2]);
  particle.rotation.z = Math.random() * 360;
  scene.add(particle);
  flameParticles.push(initParticle(particle,  currentTime + (endFade[1] / numParticles[1]) * p, 1, p));
}
for (var p = 0; p < numParticles[2]; p++) {
  var waterParticleMaterial = new THREE.MeshPhongMaterial({color: 0x3333ff, map: waterTexture, transparent: true, emissive : 0x3333ff, emissiveIntensity : 0.0, specular : 0x000000});
  var particle = new THREE.Mesh(smokeGeo,waterParticleMaterial);
  particle.material.transparent = true;
  particle.material.opacity = 1;
  particle.position.set(orbCoordinates[2][0], orbCoordinates[2][1], orbCoordinates[2][2]);
  particle.rotation.z = Math.random() * 360;
  scene.add(particle);
  waterParticles.push(initWaterParticle(particle, currentTime + (endFade[2] / numParticles[2]) * p, 2));
}
for (p = 0; p < numParticles[3]; p++) {
  var particle = new THREE.Mesh(smokeGeo,airParticleMaterial);
  particle.position.set(orbCoordinates[3][0], orbCoordinates[3][1], orbCoordinates[3][2]);
  particle.rotation.z = Math.random() * 360;
  scene.add(particle);
  windParticles.push(particle);
}
for (var p = 0; p < numParticles[4]; p++) {
  var spiritParticleMaterial = new THREE.MeshPhongMaterial({color: 0x000000, map: smokeTexture, transparent: true, emissive : 0x660066});
  var particle = new THREE.Mesh(smokeGeo,spiritParticleMaterial);
  particle.material.transparent = true;
  particle.material.opacity = 1;
  particle.position.set(orbCoordinates[4][0], orbCoordinates[4][1], orbCoordinates[4][2]);
  particle.rotation.z = Math.random() * 360;
  scene.add(particle);
  spiritParticles.push(initParticle(particle, currentTime + (endFade[4] / numParticles[4]) * p, 4));
}
for (var p = 0; p < numParticles[5]; p++) {
  var voidParticleMaterial = new THREE.MeshBasicMaterial({color: 0x000000,map: smokeTexture, transparent: true});
  var particle = new THREE.Mesh(smokeGeo,voidParticleMaterial);
  particle.material.transparent = true;
  particle.material.opacity = 1;
  particle.position.set(orbCoordinates[5][0], orbCoordinates[5][1], orbCoordinates[5][2]);
  particle.rotation.z = Math.random() * 360;
  scene.add(particle);
  voidParticles.push(initParticle(particle, currentTime + (endFade[5] / numParticles[5]) * p, 5));
}
//-------------------------------------------------------------------------------
// RESIZE FUNCTION NOT MINE
var resize = function() {
    var width = canvas.clientWidth;
    var height = canvas.clientHeight;
    if (canvas.width != width || canvas.height != height) {
          renderer.setSize(canvas.clientWidth, canvas.clientHeight, false);
        camera.aspect = canvas.clientWidth / canvas.clientHeight;
        camera.updateProjectionMatrix();
    }
};
// AND NOW BACK TO MY CODE
//-------------------------------------------------------------------------------
var addCard = function(i){
 return function(timeStep){
  cards[i].children[0].material.opacity += timeStep * .003;
  cards[i].children[1].material.opacity += timeStep * .003;
  if(cards[i].children[0].material.opacity > 1){
   cards[i].children[0].material.opacity = 1;
   cards[i].children[1].material.opacity = 1;
   return true;
  }
  else{
   return false;
  }
 }
}
var removeCard = function(i){
  return function(timeStep){
    cards[i].children[0].material.opacity -= timeStep * .003;
    cards[i].children[1].material.opacity -= timeStep * .003;
    if(cards[i].children[0].material.opacity < 0){
      cards[i].children[0].material.opacity = 0;
      cards[i].children[1].material.opacity = 0;
      return true;
    }
    else{
      return false;
    }
  };
};
var killCard = function(i){
  return function(timeStep){
    var card = cards[i];
    var foo = (180 - (card.rotation.y / Math.PI))
    card.rotation.y += (timeStep/20) * Math.PI / 180 + foo/2000;
    if(card.rotation.y > Math.PI) {
      card.rotation.y = Math.PI;
      card.position.z = 0;
      return true;
    }
    // normal z position is 100.
    card.position.z = 0 + (10 * Math.PI/2) - (10 * Math.abs(((Math.PI/2) - card.rotation.y)));
    return false;
  };
};
var reviveCard = function(i){ return function(timeStep){
  var card = cards[i];
  var foo = (180 - (card.rotation.y / Math.PI))
  card.rotation.y += (timeStep/20) * Math.PI / 180 + foo/2000;
  if(card.rotation.y > 2*Math.PI) {
    card.rotation.y = 0; 
    card.position.z = 0;
    return true;  
  }
  card.position.z = 0 + (10 * Math.PI/2) - (10 * Math.abs(((Math.PI/2) - (card.rotation.y - Math.PI))));
  return false;
};
};
//------------------------------------------------------------------------------------
var animations = [];
var lastUpdateTime = new Date().getTime();
function animate(timeStep){
  for(animation in animations){
    if(animations[animation](timeStep)){
      delete animations[animation];
    }
  }
}
//--------------------------------------------------------------------
var render = function() {
  var currentTime = new Date().getTime();
  var timeStep = currentTime - lastUpdateTime;
  lastUpdateTime = currentTime;    
  resize();
  renderer.render(scene, camera);
  requestAnimationFrame(render);
  animate(timeStep);
  for(var p = 0; p < earthParticles.length; ++p){
    updateParticle(earthParticles[p], currentTime, 0);
  }
  for(var p = 0; p < flameParticles.length; ++p){
    updateFlameParticle(flameParticles[p], currentTime);
  }
  for(var p = 0; p < waterParticles.length; ++p){
    updateWaterParticle(waterParticles[p], currentTime, 2);
  }
  for(var k = 0; k < windParticles.length; ++k){ // wind particles are sharing a material, but that's okay for now.
    if(orbLevel[3] == 0){
      windParticles[k].material.opacity = 0;
      windParticles[k].material.needsUpdate = true;
    }
    else{
      windParticles[k].material.opacity = 1;
      windParticles[k].material.needsUpdate = true;
    }
    windParticles[k].rotation.z += 0.1 * orbSpeed[3] * orbLevel[3] * timeStep * Math.random() / 100;
  }
  for(var p = 0; p < spiritParticles.length; ++p){
    updateParticle(spiritParticles[p], currentTime, 4);
  }
  for(var p = 0; p < voidParticles.length; ++p){
    updateParticle(voidParticles[p], currentTime, 5);
  }
};
render();
//---------------------------------------------------------------------
// A BIT OF THE FOLLOWING CODE IS NOT MINE
document.addEventListener( 'mousedown', onDocumentMouseDown, false );
var projector = new THREE.Projector();
function onDocumentMouseDown( event ) {
  event.preventDefault();
  var vector = new THREE.Vector3(
    ( event.clientX / document.getElementById("c").offsetWidth ) * 2 - 1,
    - ( event.clientY / document.getElementById("c").offsetHeight ) * 2 + 1,
    0.5
  );
  projector.unprojectVector( vector, camera );
  var ray = new THREE.Raycaster( camera.position, vector.sub( camera.position ).normalize() );
  for(var i = 0; i < cardMeshes.length; ++i){
    var intersects = ray.intersectObjects( [cardMeshes[i]] );
    if ( intersects.length > 0 ) {
      selectedField[i] = !selectedField[i];
      if(selectedField[i]){
        selectionMeshes[i].material.color.setHex(0xff0000 );
      }
      else{
        selectionMeshes[i].material.color.setHex(0x888888 );
      }
    }
  }
  for(var i = 0; i < plusButtons.length; ++i){
    var intersects = ray.intersectObjects( [plusButtons[i]] );
    if ( intersects.length > 0 ) {
      if(orbLevel[i] == 9){
        continue;
      }
      ++orbLevel[i];
      orbMats[i].map = positionTextures[orbLevel[i]];
    }
  }
  for(var i = 0; i < downButtons.length; ++i){
    var intersects = ray.intersectObjects( [downButtons[i]] );
    if ( intersects.length > 0 ) {
      if(orbLevel[i] == 0){
        continue;
      }
      --orbLevel[i];
      orbMats[i].map = positionTextures[orbLevel[i]];
    }
  }
}
//---------------------------------------------------------------------



