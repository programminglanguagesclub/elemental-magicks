style animateStatChangeText
style back
style backRotate
style backside
style backside____backRotate____back____card____unselected
style backside____card____back____backRotate
style ball
style ball_air
style ball_air____value_0
style ball_earth
style ball_earth____value_0
style ball_fire
style ball_fire____value_0
style ball_spirit
style ball_spirit____value_0
style ball_thoughts
style ball_thoughts____ball_thoughts_text
style ball_thoughts_text
style ball_void
style ball_void____value_0
style ball_water
style ball_water____value_0
style box
style card
style cardList
style cardListImage
style cardSkill
style cardSkillDescription
style cardStat
style centralizer
style circle
style clockDisplay
style col1
style col111
style col17777
style col1b
style col1ccc
style col1gggccc
style col1of2with0fieldstuff
style col1of3with0
style col1of3with0forskills
style col1of3with2big
style col2
style col211
style col26666
style col27777
style col2ccc
style col2gggccc
style col2of2with0fieldstuff
style col2of3with0
style col2of3with0forskills
style col2of3with2big
style col3
style col37777
style col3b
style col3ccc
style col3gggccc
style col3of3with0
style col3of3with0floatright
style col3of3with2big
style col4
style col47777
style col4b
style col4ccc
style col4gggccc
style col5
style col57777
style col5b
style col6b
style col7b
style col8b
style container0b
style container1
style container111
style container17777
style container1b
style container1ccc
style container1gggccc
style container1of2with0fieldstuff
style container1of3with0
style container1of3with2
style container1of5
style container2
style container211
style container27777
style container2ccc
style container2gggccc
style container2of2with0fieldstuff
style container2of3with0
style container2of3with2
style container2of5
style container3
style container37777
style container3b
style container3ccc
style container3gggccc
style container3of3with0
style container3of5
style container4
style container47777
style container4b
style container4ccc
style container4gggccc
style container4of5
style container5
style container57777
style container5b
style container5of5
style container5player
style container6b
style container7b
style container8b
style content
style context_box
style displayAreaSelected
style displayAreaUnselected
style displayDetails
style enemyGraveyardImage
style enemyHandImage
style enemyIcons
style face
style face____card____front
style face____front____card____unselected
style fieldCard
style font
style front
style graveyardImage
style graveyardbackground
style handImage
style iconImage
style imagWOWURWEBERRORe (*because can't name class image maybe*)
style imagWOWURWEBERRORe____graveyardbackground
style lp0
style lp1
style lp2
style lp3 (*will be removing these from the html*)
style minus_button (*changed minus-button to minus_button*)
style moreMyButton
style myButtonResign
style myButtonSmall
style outerGraveyard
style panel
style panel____displayDetails
style plus_button (*changed plus-button to plus_button*)
style positioner
style positionerMessages
style setCard
style spacing
style stage
style stage_thoughts
style thought_span
style thoughts
style unselected
style value_0
style wrapper




(*ffi svg : unit -> tag tableAttrs ([Dyn, MakeForm, Body]) ([MakeForm]) [] []*)
ffi svg : unit -> tag tableAttrs ([Dyn, MakeForm, Body]) ([MakeForm]) [] []

(*bodyTag*)

fun main () : transaction page = return <xml><head>
  <link rel="stylesheet" type="text/css" href="https://dl.dropboxusercontent.com/s/76hjjkwhiu9xz0z/earth.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/hv2uzq61xdun91j/ball.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/hadcldzyffvjzcf/water.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/z6pxjn1slszvfqv/air.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/71xwnip1zdo82mx/spirit.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/z82ld92fq1m09z6/void.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/hv2uzq61xdun91j/ball.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/fp8sjyqzhnwxtj0/plus.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/7j9wz3o184cht3t/othercss.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/7mlcwyu8ohtgj6m/clock.css?dl=0"/>
  <link rel="stylesheet" type="text/css" href="https://dl.dropbox.com/s/0kjeyzn7tb0qmzn/fire.css?dl=0"/>

(*
  <script src="./battle.jsp_files/jquery-latest.js"></script>
  <script src="./battle.jsp_files/jquery-latest(1).js"></script>
  <script src="./battle.jsp_files/cardStat.js"></script>
  <script src="./battle.jsp_files/knowledge.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/battle.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/cardList.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/tutorial.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/clock.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/gameData.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/gameInterface.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/phase.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/updateFunctions.js" type="text/javascript"></script>
  <script src="./battle.jsp_files/userInput.js" type="text/javascript"></script>
  <script type="text/javascript" src="./battle.jsp_files/jsapi"></script><style type="text/css"></style>
  <script language="javascript">  
  
    If I keep a unit up until it is the next unit's turn, it creates the impression that the unit is not engaged immediately after it executes an ability,
    even though that is what I have implemented on the server.
 
  </script>


*)
 </head>
 <body (* id="active2" *) (* style="display: block; overflow: hidden;" *)>
  (*<div (* id="loadingScreen" *) (* style="background-color: #000;width:100%; height:100%; text-align:center; color:white; z-index:6000000; position:fixed;right:0;top:0;display:block;" *)>
   <span (* id="loadingScreenText" *) (* style="textalign:center;font-size:42px;" *)>Opponent Still Connecting.</span>
   <br>
   <button class={myButtonSame} (* id="returnToLobby" *) (* style="font-size:35px;width:50%;" *) (* onclick="AttemptToReturnToLobby();" *)>Return to Lobby</button>
  </div>*)
  <div style="position:relative;height:98%;">
   <div style="object-fit: contain; height:100%; width:100%;">
    <span class={wrapper}>
     <svg style = "width:11580px;height:9000px" class={imagWOWURWEBERRORe}></svg>
     <div class={positioner} style="background-color:#404040;">
      <div style="padding-top:77.7%;">
       <div class={context_box}>
        <div class={content}>
         <div class={container2of3with2}>
          <div class={container1of3with2}>
           <div class={col1of3with2big}>
            <span class={enemyIcons} style="visibility:hidden;">
             <div class={container3of3with0}>
              <div class={container2of3with0}>
               <div class={container1of3with0}>
                <div class={col1of3with0}>
                 <div (* id="enemygraveyardbutton" *) title="Displays the cards in your opponent&#39;s graveyard" class={displayAreaUnselected} (*style="width:50%; float:left; border-radius:25%; -webkit-transform:translate(50%,0%);" *) (* onclick="selectEnemyGraveyard();" *)>
                  <img class={iconImage} src="https://dl.dropbox.com/s/uh7zvgn0v2f5bex/grave.png?dl=0"/>
                 </div>
                </div>
                <div class={col2of3with0}>
                 <div (* id="enemyhandbutton" *) title="Displays the cards in your opponent&#39;s hand" class={displayAreaSelected} (* style="width:50%; float:left; border-radius:25%; -webkit-transform:translate(50%,0%);" *) (* onclick="selectEnemyHand();" *)>
                  <img class={iconImage} src="https://dl.dropbox.com/s/20fp1fihiowyrtl/hand.png?dl=0"/>
                 </div>
                </div>
                <div class={col3of3with0}>
                 <div (* id="enemyfieldbutton" *) title="Displays your opponent&#39;s field and set position" class={displayAreaUnselected} (* style="width:50%;float:left; border-radius:25%; -webkit-transform:translate(50%,0%);" *) (* onclick="selectEnemyField();" *)>
                  <img class={iconImage} src="https://dl.dropbox.com/s/dweirm7vekugask/field.png?dl=0"/>
                 </div>
                </div>
               </div>
              </div>
             </div>
            </span>
            <div class={outerGraveyard}>
             <span class={wrapper}>
              <svg style="width:500px;height:500px;" class={imagWOWURWEBERRORe____graveyardbackground}></svg> *)
              <div class={positioner}>
               <div style="padding-top: 100%;">
                <div class={context_box}>
                 <div class={content}>
                  <div>
                   <div (* id="containerEnemyGraveyard" *) class={container5of5} style="display:none;">
                    <div class={container4of5}>
                     <div class={container3of5}>
                      <div class={container2of5}>
                       <div class={container1of5}>
                        <div class={col1}>
                         <div (* id="enemygraveyard0" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="0" *) class={enemyGraveyardImage} (* id="enemygraveyardImage0" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard5" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="5" *) class={enemyGraveyardImage} (* id="enemygraveyardImage5" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard10" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="10" *) class={enemyGraveyardImage} (* id="enemygraveyardImage10" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard15" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="15" *) class={enemyGraveyardImage} (* id="enemygraveyardImage15" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard20" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="20" *) class={enemyGraveyardImage} (* id="enemygraveyardImage20" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col2}>
                         <div (* id="enemygraveyard1" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="1" *) class={enemyGraveyardImage} (* id="enemygraveyardImage1" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard6" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="6" *) class={enemyGraveyardImage} (* id="enemygraveyardImage6" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard11" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="11" *) class={enemyGraveyardImage} (* id="enemygraveyardImage11" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard16" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="16" *) class={enemyGraveyardImage} (* id="enemygraveyardImage16" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard21" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="21" *) class={enemyGraveyardImage} (* id="enemygraveyardImage21" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col3}>
                         <div (* id="enemygraveyard2" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="2" *) class={enemyGraveyardImage} (* id="enemygraveyardImage2" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard7" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="7" *) class={enemyGraveyardImage} (* id="enemygraveyardImage7" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard12" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="12" *) class={enemyGraveyardImage} (* id="enemygraveyardImage12" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard17" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="17" *) class={enemyGraveyardImage} (* id="enemygraveyardImage17" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard22" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="22" *) class={enemyGraveyardImage} (* id="enemygraveyardImage22" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col4}>
                         <div (* id="enemygraveyard3" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="3" *) class={enemyGraveyardImage} (* id="enemygraveyardImage3" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard8" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="8" *) class={enemyGraveyardImage} (* id="enemygraveyardImage8" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard13" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="13" *) class={enemyGraveyardImage} (* id="enemygraveyardImage13" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard18" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="18" *) class={enemyGraveyardImage} (* id="enemygraveyardImage18" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard23" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="23" *) class={enemyGraveyardImage} (* id="enemygraveyardImage23" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col5}>
                         <div (* id="enemygraveyard4" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="4" *) class={enemyGraveyardImage} (* id="enemygraveyardImage4" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard9" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="9" *) class={enemyGraveyardImage} (* id="enemygraveyardImage9" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard14" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="14" *) class={enemyGraveyardImage} (* id="enemygraveyardImage14" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard19" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="19" *) class={enemyGraveyardImage} (* id="enemygraveyardImage19" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard24" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="24" *) class={enemyGraveyardImage} (* id="enemygraveyardImage24" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                   <div class={container4gggccc} style="display:none;">
                    <div class={container3gggccc}>
                     <div class={container2gggccc}>
                      <div class={container1gggccc}>
                       <div class={col1gggccc}>
(*____ means that I need to make a new css class that is the union of the two, to satisfy UrWeb's one class policy*)
                        <div (* id="enemy8" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(8)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/8.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy8Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,8);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy8" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy8Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy8Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget8" *) class={circle} style="display:none;"></figure>
                        </div>
                        <div (* id="enemy5" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(5)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/5.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy5Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,5);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy5" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy5Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy5Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget5" *) class={circle} style="display:none;"></figure>
                        </div>
                        <div (* id="enemy2" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(2)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/2.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy2Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,2);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy2" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy2Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy2Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget2" *) class={circle} style="display:none;"></figure>
                        </div>
                       </div>
                       <div class={col2gggccc}>
                        <div (* id="enemy7" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(7)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/7.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy7Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,7);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy7" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy7Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy7Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp"  style="width:90%; padding:5%; border-radius:20%;" />
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;" />
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget7" *) class={circle} style="display:none;" ></figure>
                        </div>
                        <div (* id="enemy4" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(4)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/4.png" style="width:90%; padding:5%; border-radius:20%;" />
                         </div>
                         <span (* id="enemy4Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,4);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy4" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy4Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy4Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget4" *) class={circle} style="display:none;"></figure>
                        </div>
                        <div (* id="enemy1" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(1)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/1.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy1Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,1);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy1" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy1Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy1Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget1" *) class={circle} style="display:none;"></figure>
                        </div>
                       </div>
                       <div class={col3gggccc}>
                        <div (* id="enemy6" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(6)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/6.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy6Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,6);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy6" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy6Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy6Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget6" *) class={circle}></figure>
                        </div>
                        <div (* id="enemy3" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(3)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/3.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy3Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,3);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy3" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy3Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy3Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}>
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget3" *) class={circle}></figure>
                        </div>
                        <div (* id="enemy0" *) class={panel____displayDetails} (* onclick="phase.clickEnemyField(0)" *) style="margin-bottom:28%; width:100%;">
                         <div class={unselected}>
                          <img src="./battle.jsp_files/0.png" style="width:90%; padding:5%; border-radius:20%;"/>
                         </div>
                         <span (* id="enemy0Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(1,0);" *)>
                          <div class={face____card____front}>
                           <div class={unselected}>
                            <div class={animateStatChangeText} (* id="statAnimationEnemy0" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                            <div (* id="enemy0Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                            <img (* id="enemy0Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                          <div class={backside____card____back____backRotate}>
                           <div class={unselected}> 
                            <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                          </div>
                         </span>
                         <figure (* id="enemyNextTarget0" *) class={circle}></figure>
                        </div>
                       </div>
                       <div class={col4gggccc}>
                        <div class={container2of2with0fieldstuff}>
                         <div class={container1of2with0fieldstuff}>
                          <div class={col1of2with0fieldstuff}>
                           <button (* id="row2" *) (* onclick="userInput.selectRow(2);" *) class={moreMyButton} (* style="visibility:hidden; width:150%; padding-top:150%;-webkit-transform:translate(-45%,-310%);" *)></button>
                           <button (* id="row1" *) (* onclick="userInput.selectRow(1);" *) class={moreMyButton} (* style="visibility:hidden; width:150%; padding-top:150%;-webkit-transform:translate(-45%,0%);" *)></button>
                           <button (* id="row0" *) (* onclick="userInput.selectRow(0);" *) class={moreMyButton} (* style="visibility:hidden; width:150%; padding-top:150%;-webkit-transform:translate(-45%,340%);" *)></button>
                          </div>
                          <div class={col2of2with0fieldstuff}>
                           <div (* id="enemySet" *) class={panel____displayDetails}>
                            <div class={unselected}>
                             <img src="https://dl.dropbox.com/s/6bd0ni2hpd0uqwy/set.png?dl=0" style="width:90%; padding:5%; border-radius:20%;"/>
                            </div>
                            <span (* id="enemySetCard" *) style="visibility:hidden;" class={setCard} (* onmouseover="displayDetailsSet(1);" *)>
                             <div class={face____card____front}>
                              <div class={unselected}>
                               <img (* id="enemySetImage" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                              </div>
                             </div>
                             <div class={backside____card____back____backRotate}>
                              <div class={unselected}>
                               <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                              </div>
                             </div>
                            </span>
                           </div>
                          </div>
                         </div>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                   <div (* id="container5enemyhand" *) style="display:block;">
                    <div class={container4}>
                     <div class={container3}>
                      <div class={container2}>
                       <div class={container1}>
                        <div class={col1}>
                         <div (* id="enemyhand0" *) style="visibility:hidden; border-radius:10%; margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="0" *) class={enemyHandImage} (* id="enemyhandImage0" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand5" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="5" *) class={enemyHandImage} (* id="enemyhandImage5" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand10" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="10" *) class={enemyHandImage} (* id="enemyhandImage10" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand15" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="15" *) class={enemyHandImage} (* id="enemyhandImage15" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand20" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="20" *) class={enemyHandImage} (* id="enemyhandImage20" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col2}>
                         <div (* id="enemyhand1" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="1" *) class={enemyHandImage} (* id="enemyhandImage1" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand6" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="6" *) class={enemyHandImage} (* id="enemyhandImage6" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand11" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="11" *) class={enemyHandImage} (* id="enemyhandImage11" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand16" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="16" *) class={enemyHandImage} (* id="enemyhandImage16" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand21" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="21" *) class={enemyHandImage} (* id="enemyhandImage21" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col3}>
                         <div (* id="enemyhand2" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="2" *) class={enemyHandImage} (* id="enemyhandImage2" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand7" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="7" *) class={enemyHandImage} (* id="enemyhandImage7" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand12" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="12" *) class={enemyHandImage} (* id="enemyhandImage12" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand17" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="17" *) class={enemyHandImage } (* id="enemyhandImage17" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand22" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="22" *) class={enemyHandImage} (* id="enemyhandImage22" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col4}>
                         <div (* id="enemyhand3" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="3" *) class={enemyHandImage} (* id="enemyhandImage3" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand8" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="8" *) class={enemyHandImage} (* id="enemyhandImage8" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand13" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="13" *) class={enemyHandImage} (* id="enemyhandImage13" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand18" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="18" *) class={enemyHandImage} (* id="enemyhandImage18" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand23" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="23" *) class={enemyHandImage} (* id="enemyhandImage23" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                        <div class={col5}>
                         <div (* id="enemyhand4" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="4" *) class={enemyHandImage} (* id="enemyhandImage4" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand9" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="9" *) class={enemyHandImage} (* id="enemyhandImage9" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand14" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="14" *) class={enemyHandImage} (* id="enemyhandImage14" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand19" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="19" *) class={enemyHandImage} (* id="enemyhandImage19" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemyhand24" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="24" *) class={enemyHandImage} (* id="enemyhandImage24" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </div>
                 </div>
                </div>
               </div>
              </div>
             </span>
            </div>
            <div class={outerGraveyard}>
             <span class={wrapper}>
             <svg style="width:500px;height:500px;" class={imagWOWURWEBERRORe____graveyardbackground}></svg>
              <div class={positioner}>
               <div style="padding-top: 100%;">
                <div class={context_box}>
                 <div class={content}>
                  <div class={container4ccc} style="display:none;">
                   <div class={container3ccc}>
                    <div class={container2ccc}>
                     <div class={container1ccc}>
                      <div class={col1ccc}>
                       <div (* id="player0" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(0);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/0.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player0Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,0);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer0" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player0Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player0Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget0" *) class={circle}></figure>
                       </div>
                       <div (* id="player3" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(3);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/3.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player3Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,3);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer3" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player3Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player3Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget3" *) class={circle}></figure>
                       </div>
                       <div (* id="player6" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(6);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/6.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player6Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,6);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer6" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player6Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player6Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget6" *) class={circle}></figure>
                       </div>
                      </div>
                      <div class={col2ccc}>
                       <div (* id="player1" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(1);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/1.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player1Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,1);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer1" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player1Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player1Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget1" *) class={circle} style="display:none;"></figure>
                       </div>
                       <div (* id="player4" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(4);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/4.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player4Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,4);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer4" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player4Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player4Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget4" *) class={circle} style="display:none;"></figure>
                       </div>
                       <div (* id="player7" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(7);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/7.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player7Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,7);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer7" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player7Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player7Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget7" *) class={circle} style="display:none;"></figure>
                       </div>
                      </div>
                      <div class={col3ccc}>
                       <div (* id="player2" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(2);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/2.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player2Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,2);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer2" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player2Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player2Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget2" *) class={circle} style="display:none;"></figure>
                       </div>
                       <div (* id="player5" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(5);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}>
                         <img src="./battle.jsp_files/5.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player5Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,5);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer5" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player5Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player5Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget5" *) class={circle} style="display:none;"></figure>
                       </div>
                       <div (* id="player8" *) class={panel____displayDetails} (* onclick="phase.clickPlayerField(8);" *) style="margin-bottom:28%; width:100%;">
                        <div class={unselected}> 
                         <img src="./battle.jsp_files/8.png" style="width:90%; padding:5%; border-radius:20%;"/>
                        </div>
                        <span (* id="player8Card" *) style="visibility:hidden;" class={fieldCard} (* onmouseover="displayDetailsField(0,8);" *)>
                         <div class={face____card____front}>
                          <div class={unselected}>
                           <div class={animateStatChangeText} (* id="statAnimationPlayer8" *) style="visibility: visible; position: absolute; width: 100%; padding-bottom: 125%; z-index: 1000; font-size: 8.235px;"></div>
                           <div (* id="player8Engaged" *) style="visibility:hidden;opacity:.5;background-color:black;position:absolute;z-index:50;width:100%;padding-top:125%;"></div>
                           <img (* id="player8Image" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                         <div class={backside____card____back____backRotate}>
                          <div class={unselected}>
                           <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                          </div>
                         </div>
                        </span>
                        <figure (* id="playerNextTarget8" *) class={circle} style="display:none;"></figure>
                       </div>
                      </div>
                      <div (* id="col4ccc" *)>
                       <div class={container2of2with0fieldstuff}>
                        <div class={container1of2with0fieldstuff}>
                         <div class={col1of2with0fieldstuff}>
                          <button class={moreMyButton} (* style="visibility:hidden; width:150%; padding-top:150%;-webkit-transform:translate(-45%,-310%);" *)></button>
                          <button class={moreMyButton} (* style="visibility:hidden; width:150%; padding-top:150%;-webkit-transform:translate(-45%,0%);" *)></button>
                          <button class={moreMyButton} (* style="visibility:hidden; width:150%; padding-top:150%;-webkit-transform:translate(-45%,340%);" *)></button>
                         </div>
                         <div class={col2of2with0fieldstuff}>
                          <div (* id="playerSet" *) class={panel____displayDetails}>
                           <div class={unselected}>
                            <img src="https://dl.dropbox.com/s/6bd0ni2hpd0uqwy/set.png?dl=0" style="width:90%; padding:5%; border-radius:20%;"/>
                           </div>
                           <span (* id="playerSetCard" *) style="visibility:hidden;" class={setCard} (* onmouseover="displayDetailsSet(0);" *)>
                            <div class={face____card____front}>
                             <div class={unselected}>
                              <img (* id="playerSetImage" *) src="http://elementalmagickscardgame.appspot.com/battle.jsp" style="width:90%; padding:5%; border-radius:20%;"/>
                             </div>
                            </div>
                            <div class={backside____card____back____backRotate}>
                             <div class={unselected}>
                              <img src="./battle.jsp_files/cardback.png" style="width:90%; padding:5%; border-radius:20%;"/>
                             </div>
                            </div>
                           </span>
                          </div>
                         </div>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </div>
                  <div class={container5player} style="display:none;">
                   <div class={container4}>
                    <div class={container3}>
                     <div class={container2}>
                      <div class={container1}>
                       <div class={col1}>
                        <div (* id="graveyard0" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="0" *) class={graveyardImage} (* id="graveyardImage0" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard5" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="5" *) class={graveyardImage} (* id="graveyardImage5" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard10" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="10" *) class={graveyardImage} (* id="graveyardImage10" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard15" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="15" *) class={graveyardImage} (* id="graveyardImage15" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard20" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="20" *) class={graveyardImage} (* id="graveyardImage20" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col2}>
                        <div (* id="graveyard1" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="1" *) class={graveyardImage} (* id="graveyardImage1" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard6" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="6" *) class={graveyardImage} (* id="graveyardImage6" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard11" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="11" *) class={graveyardImage} (* id="graveyardImage11" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard16" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="16" *) class={graveyardImage} (* id="graveyardImage16" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard21" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="21" *) class={graveyardImage} (* id="graveyardImage21" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col3}>
                        <div (* id="graveyard2" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="2" *) class={graveyardImage} (* id="graveyardImage2" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard7" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="7" *) class={graveyardImage} (* id="graveyardImage7" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard12" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="12" *) class={graveyardImage} (* id="graveyardImage12" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard17" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="17" *) class={graveyardImage} (* id="graveyardImage17" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard22" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="22" *) class={graveyardImage} (* id="graveyardImage22" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col4}>
                        <div (* id="graveyard3" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="3" *) class={graveyardImage} (* id="graveyardImage3" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard8" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="8" *) class={graveyardImage} (* id="graveyardImage8" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard13" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="13" *) class={graveyardImage} (* id="graveyardImage13" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard18" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="18" *) class={graveyardImage} (* id="graveyardImage18" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard23" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="23" *) class={graveyardImage} (* id="graveyardImage23" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col5}>
                        <div (* id="graveyard4" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="4" *) class={graveyardImage} (* id="graveyardImage4" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard9" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="9" *) class={graveyardImage} (* id="graveyardImage9" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard14" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="14" *) class={graveyardImage} (* id="graveyardImage14" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard19" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="19" *) class={graveyardImage} (* id="graveyardImage19" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="graveyard24" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="24" *) class={graveyardImage} (* id="graveyardImage24" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </div>
                  <div (* id="container5playercardlist" *) style="display:block;">
                   <div class={container4}>
                    <div class={container3}>
                     <div class={container2}>
                      <div class={container1}>
                       <div class={col1}>
                        <div (* id="cardList0" *) style="visibility:hidden; border-radius:10%; margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="0" *) class={cardListImage} (* id="cardListImage0" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList5" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="5" *) class={cardListImage} (* id="cardListImage5" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList10" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="10" *) class={cardListImage} (* id="cardListImage10" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList15" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="15" *) class={cardListImage} (* id="cardListImage15" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList20" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="20" *) class={cardListImage} (* id="cardListImage20" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col2}>
                        <div (* id="cardList1" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="1" *) class={cardListImage} (* id="cardListImage1" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList6" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="6" *) class={cardListImage} (* id="cardListImage6" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList11" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="11" *) class={cardListImage} (* id="cardListImage11" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList16" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="16" *) class={cardListImage} (* id="cardListImage16" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList21" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="21" *) class={cardListImage} (* id="cardListImage21" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col3}>
                        <div (* id="cardList2" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="2" *) class={cardListImage} (* id="cardListImage2" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList7" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="7" *) class={cardListImage} (* id="cardListImage7" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList12" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="12" *) class={cardListImage} (* id="cardListImage12" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList17" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="17" *) class={cardListImage} (* id="cardListImage17" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList22" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="22" *) class={cardListImage} (* id="cardListImage22" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col4}>
                        <div (* id="cardList3" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="3" *) class={cardListImage} (* id="cardListImage3" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList8" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="8" *) class={cardListImage} (* id="cardListImage8" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList13" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="13" *) class={cardListImage} (* id="cardListImage13" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList18" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="18" *) class={cardListImage} (* id="cardListImage18" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList23" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="23" *) class={cardListImage} (* id="cardListImage23" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col5}>
                        <div (* id="cardList4" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="4" *) class={cardListImage} (* id="cardListImage4" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList9" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="9" *) class={cardListImage} (* id="cardListImage9" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList14" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="14" *) class={cardListImage} (* id="cardListImage14" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList19" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="19" *) class={cardListImage} (* id="cardListImage19" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="cardList24" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="24" *) class={cardListImage} (* id="cardListImage24" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </div>
                  <div (* id="container5playerhand" *) style="display:none;">
                   <div class={container4}>
                    <div class={container3}>
                     <div class={container2}>
                      <div class={container1}>
                       <div class={col1}>
                        <div (* id="hand0" *) style="visibility:hidden; border-radius:10%; margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="0" *) (* player="0" *) class={handImage} (* id="handImage0" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand5" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="5" *) (* player="0" *) class={handImage} (* id="handImage5" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand10" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="10" *) (* player="0" *) class={handImage} (* id="handImage10" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand15" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="15" *) (* player="0" *) class={handImage} (* id="handImage15" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand20" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="20" *) (* player="0" *) class={handImage} (* id="handImage20" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col2}>
                        <div (* id="hand1" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="1" *) (* player="0" *) class={handImage} (* id="handImage1" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand6" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="6" *) (* player="0" *) class={handImage} (* id="handImage6" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand11" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="11" *) (* player="0" *) class={handImage} (* id="handImage11" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand16" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="16" *) (* player="0" *) class={handImage} (* id="handImage16" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand21" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="21" *) (* player="0" *) class={handImage} (* id="handImage21" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col3}>
                        <div (* id="hand2" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="2" *) (* player="0" *) class={handImage} (* id="handImage2" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand7" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="7" *) (* player="0" *) class={handImage} (* id="handImage7" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand12" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="12" *) (* player="0" *) class={handImage} (* id="handImage12" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand17" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="17" *) (* player="0" *) class={handImage} (* id="handImage17" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand22" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="22" *) (* player="0" *) class={handImage} (* id="handImage22" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col4}>
                        <div (* id="hand3" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="3" *) (* player="0" *) class={handImage} (* id="handImage3" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand8" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="8" *) (* player="0" *) class={handImage} (* id="handImage8" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand13" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="13" *) (* player="0" *) class={handImage} (* id="handImage13" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand18" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="18" *) (* player="0" *) class={handImage} (* id="handImage18" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand23" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="23" *) (* player="0" *) class={handImage} (* id="handImage23" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                       <div class={col5}>
                        <div (* id="hand4" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="4" *) (* player="0" *) class={handImage} (* id="handImage4" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand9" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="9" *) (* player="0" *) class={handImage} (* id="handImage9" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand14" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="14" *) (* player="0" *) class={handImage} (* id="handImage14" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand19" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="19" *) (* player="0" *) class={handImage} (* id="handImage19" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                        <div (* id="hand24" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                         <img (* index="24" *) (* player="0" *) class={handImage} (* id="handImage24" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </div>
                 </div>
                </div>
               </div>
              </div>
             </span>
            </div>
            <span (* id="playIcons" *) style="display:none;">
             <div class={container3of3with0}>
              <div class={container2of3with0}>
               <div class={container1of3with0}>
                <div class={col1of3with0}>
                 <div (* id="playergraveyardbutton" *) title="Displays the cards in your graveyard" class={displayAreaSelected} (* style="width:50%; float:left; border-radius:25%; -webkit-transform:translate(50%,0%);" *) (* onclick="selectPlayerGraveyard();" *)>
                  <img class={iconImage} src="https://dl.dropbox.com/s/uh7zvgn0v2f5bex/grave.png?dl=0"/>
                 </div>
                </div>
                <div class={col2of3with0}>
                 <div (* id="playerhandbutton2" *) title="Displays the cards in your hand" class={displayAreaUnselected} (* style="width:50%; float:left; border-radius:25%; -webkit-transform:translate(50%,0%);" *) (* onclick="selectPlayerHand();" *)>
                  <img class={iconImage} src="https://dl.dropbox.com/s/20fp1fihiowyrtl/hand.png?dl=0"/>
                 </div>
                </div>
                <div class={col3of3with0}>
                 <div (* id="playerfieldbutton" *) title="Displays your field and set position" class={displayAreaUnselected} (* style="width:50%;float:left; border-radius:25%; -webkit-transform:translate(50%,0%);" *) (* onclick="selectPlayerField();" *)>
                  <img class={iconImage} src="https://dl.dropbox.com/s/dweirm7vekugask/field.png?dl=0"/>
                 </div>
                </div>
               </div>
              </div>
             </div>
            </span>
            <div class={container57777}>
             <div class={container47777}>
              <div class={container37777}>
               <div class={container27777}>
                <div class={container17777}>
                 <div class={col17777}>
                  <div (* id="cardListLeft" *) class={cardList} (* style="top: 50%; font-size: 32.94px;" *) (* onclick="cardListLeft();" *) title="View previous page">➤</div>
                 </div>
                 <div class={col27777}>
                  <div (* id="playerCardListButton" *) class={displayAreaSelected} style=" border-radius:25%;" (* onclick="selectCardList();" *) title="View all cards in the game that matched your last card search">
                   <img class={iconImage} src="https://dl.dropbox.com/s/82u372xibrpooz9/List.png?dl=0"/>
                  </div>
                 </div>
                 <div class={col37777}>
                  <div (* id="cardListRight" *) class={cardList} (* style="top: 50%; font-size: 32.94px;" *) (* onclick="cardListRight();" *) title="View next page">➤</div>
                 </div>
                 <div class={col47777}>
                  <div (* id="playerhandbutton" *) title="Displays the cards in your hand" class={displayAreaUnselected} style=" border-radius:25%;" (* onclick="selectPlayerHand();" *)>
                   <img class={iconImage} src="https://dl.dropbox.com/s/20fp1fihiowyrtl/hand.png?dl=0"/>
                  </div>
                 </div>
                 <div class={col57777}>
                  <div (* id="drawcardbutton" *) title="Draws the currently selected card from the card list to either your hand, or, every six draws, your selected position among your soul cards" class={displayAreaUnselected} style=" border-radius:25%;" (* onclick="drawCard();" *)>
                   <img class={iconImage} src="./battle.jsp_files/3h69qP1404547095.png"/>
                  </div>
                 </div>
                </div>
               </div>
              </div>
             </div>
            </div>
           </div>
           <div class={col2of3with2big}>
            <span (* id="nhhh" *) (* style="float:left;background: rgba(0, 0, 0, 1.0); width:100%; padding:1%; border-radius:10%;" *)>
             <div (* id="container5" *) (* style="background: rgba(200, 230, 255, 0.4); padding-bottom:1%; border-radius:5%; width:98%;" *)>
              <div class={container4}>
               <div class={container3}>
                <div class={container2}>
                 <div class={container1}>
                  <div class={col1}>
                   <div (* onclick="phase.clickEnemySoul(4)" *) (* id="enemySoul4" *) title="The fifth and last position in your opponent&#39;s soul cards. The card placed here will be the last to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points. If your opponent loses any soul points beyond this, you will win the round.">
                    <div (* id="enemySoulContainer4" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                     <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulFace4" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img (* id="enemySoulImage4" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulBack4" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                   </div>
                  </div>
                  <div class={col2}>
                   <div (* onclick="phase.clickEnemySoul(3)" *) (* id="enemySoul3" *) title="The fourth position in your opponent&#39;s soul cards. The card placed here will be the fourth to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                    <div (* id="enemySoulContainer3" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                     <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulFace3" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img (* id="enemySoulImage3" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulBack3" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                   </div>
                  </div>
                  <div class={col3}>
                   <div (* onclick="phase.clickEnemySoul(2)" *) (* id="enemySoul2" *) title="The third position in your opponent&#39;s soul cards. The card placed here will be the third to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                    <div (* id="enemySoulContainer2" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                     <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulFace2" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img (* id="enemySoulImage2" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulBack2" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                   </div>
                  </div>
                  <div class={col4}>
                   <div (* onclick="phase.clickEnemySoul(1)" *) (* id="enemySoul1" *) title="The second position in your opponent&#39;s soul cards. The card placed here will be the second to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                    <div (* id="enemySoulContainer1" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                     <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulFace1" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img (* id="enemySoulImage1" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulBack1" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                     <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                   </div>
                  </div>
                  <div class={col5}>
                   <div (* onclick="phase.clickEnemySoul(0)" *) (* id="enemySoul0" *) title="The first position in your opponent&#39;s soul cards. The card placed here will be the first to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                    <div (* id="enemySoulContainer0" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                     <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulFace0" *) class={face____front____card____unselected} (* style="border-radius:10%; margin-top:4%; margin-bottom:0%; display:none;" *)>
                     <img (* id="enemySoulImage0" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                    <div (* id="enemySoulBack0" *) class={backside____backRotate____back____card____unselected} (* style="border-radius:10%; margin-top:4%; margin-bottom:0%; display:none;" *)>
                     <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                    </div>
                   </div>
                  </div>
                 </div>
                </div>
               </div>
              </div>
             </div>
            </span>
            <div style="width:98%; padding-top:.5%; padding-bottom:.5%; padding-left:.5%; padding-right:1.5%; background-color:#444c4f; display:inline-block;">
            <div style="width:100%; padding:1%; background-color:#222222;">
             <div style="width:100%; background-color:black; padding:0%;">
              <span (* id="enemylp4-0-wrapper" *) style="display:inline;">
              <div class={lp0}></div>
              </span>
              <span (* id="enemylp4-1-wrapper" *) style="display:none;">
               <div class={lp1} (* id="enemylp4-1-1" *)></div>
              </span>
              <span (* id="enemylp4-2-wrapper" *) style="display:none;"><div class={lp2} (* id="enemylp4-2-1" *)></div><div class={spacing}></div><div class={lp2} (* id="enemylp4-2-2" *)></div></span><span (* id="enemylp4-3-wrapper" *) style="display:none;"><div class={lp3} (* id="enemylp4-3-1" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp4-3-2" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp4-3-3" *)></div></span><div class={spacing}></div><span (* id="enemylp3-0-wrapper" *) style="display:inline;"><div class={lp0}></div></span><span (* id="enemylp3-1-wrapper" *) style="display:none;"><div class={lp1} (* id="enemylp3-1-1" *)></div></span><span (* id="enemylp3-2-wrapper" *) style="display:none;"><div class={lp2} (* id="enemylp3-2-1" *)></div><div class={spacing}></div><div class={lp2} (* id="enemylp3-2-2" *)></div></span><span (* id="enemylp3-3-wrapper" *) style="display:none;"><div class={lp3} (* id="enemylp3-3-1" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp3-3-2" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp3-3-3" *)></div></span><div class={spacing}></div><span (* id="enemylp2-0-wrapper" *) style="display:inline;"><div class={lp0}></div></span><span (* id="enemylp2-1-wrapper" *) style="display:none;"><div class={lp1} (* id="enemylp2-1-1" *)></div></span><span (* id="enemylp2-2-wrapper" *) style="display:none;"><div class={lp2} (* id="enemylp2-2-1" *)></div><div class={spacing}></div><div class={lp2} (* id="enemylp2-2-2" *)></div></span><span (* id="enemylp2-3-wrapper" *) style="display:none;"><div class={lp3} (* id="enemylp2-3-1" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp2-3-2" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp2-3-3" *)></div></span><div class={spacing}></div><span (* id="enemylp1-0-wrapper" *) style="display:inline;"><div class={lp0}></div></span><span (* id="enemylp1-1-wrapper" *) style="display:none;"><div class={lp1} (* id="enemylp1-1-1" *)></div></span><span (* id="enemylp1-2-wrapper" *) style="display:none;"><div class={lp2} (* id="enemylp1-2-1" *)></div><div class={spacing}></div><div class={lp2} (* id="enemylp1-2-2" *)></div></span><span (* id="enemylp1-3-wrapper" *) style="display:none;"><div class={lp3} (* id="enemylp1-3-1" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp1-3-2" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp1-3-3" *)></div></span><div class={spacing}></div><span (* id="enemylp0-0-wrapper" *) style="display:inline;"><div class={lp0}></div></span><span (* id="enemylp0-1-wrapper" *) style="display:none;"><div class={lp1} (* id="enemylp0-1-1" *)></div></span><span (* id="enemylp0-2-wrapper" *) style="display:none;"><div class={lp2} (* id="enemylp0-2-1" *)></div><div class={spacing}></div><div class={lp2} (* id="enemylp0-2-2" *)></div></span><span (* id="enemylp0-3-wrapper" *) style="display:none;"><div class={lp3} (* id="enemylp0-3-1" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp0-3-2" *)></div><div class={spacing}></div><div class={lp3} (* id="enemylp0-3-3" *)></div></span>
             </div>
            </div>
           </div>
           <span class={wrapper} (* id="cardarea" *)>
            <svg style="width:1600px;height:2000px;" class={imagWOWURWEBERRORe}></svg>
            <div class={positioner}>
             <div style="padding-top: 125%;">
              <div class={context_box}>
               <div class={content}>
                <div (* id="nondisplaycard" *)>
                 <div (* id="enemyResources" *) style="visibility:hidden;">
                  <div class={container8b}>
                   <div class={container7b}>
                    <div class={container6b}>
                     <div class={container5b}>
                      <div class={container4b}>
                       <div class={container3b}>
                        <div class={container1b}>
                         <div class={container0b}>
                          <div class={col1b}>
                           <section class={stage_thoughts}>
                            <figure class={ball_thoughts} title="Enemy thoughts" (* style="font-size: 4.392px;" *)>
                             <span (* id="enemyThoughtSpan" *) class={thought_span} (* style="overflow: hidden; text-align: center; transform: translate(0%, -45%); font-size: 21.96px;" *)>5</span> 
                            </figure>
                           </section>
                          </div>
                          <div class={col3b} style="padding-top:4.5%; padding-left:7%; padding-right:1%;">
                            <section class={stage}>
                             <figure (* id="enemyBall1" *) class={ball_earth____value_0} style="text-align: center; font-size: 16.47px;" title="Enemy earth knowledge"></figure>
                            </section>
                           </div>
                           <div class={col4b} (* style="padding-top:4.5%; padding-left:1%; padding-right:1%;" *)>
                            <section class={stage}>
                             <figure (* id="enemyBall2" *) class={ball_fire____value_0} style="text-align: center; font-size: 16.47px;" title="Enemy fire knowledge"></figure>
                            </section>
                           </div>
                           <div class={col5b} (* style="padding-top:4.5%; padding-left:1%; padding-right:1%;" *)>
                            <section class={stage}>
                             <figure (* id="enemyBall3" *) class={ball_water____value_0} style="text-align: center; font-size: 16.47px;" title="Enemy water knowledge"></figure>
                            </section>
                           </div>
                           <div class={col6b} (* style="padding-top:4.5%; padding-left:1%; padding-right:1%;" *)>
                            <section class={stage}>
                             <figure (* id="enemyBall4" *) class={ball_air____value_0} style="text-align: center; font-size: 16.47px;" title="Enemy air knowledge"></figure>
                            </section>
                           </div>
                           <div class={col7b} (* style="padding-top:4.5%; padding-left:1%; padding-right:1%;" *)>
                            <section class={stage}>
                             <figure (* id="enemyBall5" *) class={ball_spirit____value_0} style="text-align: center; font-size: 16.47px;" title="Enemy spirit knowledge"></figure>
                            </section>
                           </div>
                           <div class={col8b} (* style="padding-top:4.5%; padding-left:1%; padding-right:1%;" *)>
                            <section class={stage}>
                             <figure (* id="enemyBall6" *) class={ball_void____value_0} style="text-align: center; font-size: 16.47px;" title="Enemy void knowledge"></figure>
                            </section>
                           </div>
                          </div>
                         </div>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </div>


(*
                  <div class={container26666"} style = "width:100%; padding-top:50%;height:10%;">
                   <div class={container16666"}>
                    <div class={col16666"}>
                     <span (* id="cardSearchAreaOther" *) style="display:none;">
                      <button style="visibility: hidden; font-size: 14.47px; width:50%; height:100%;" class={myButton}></button> 
                      <button (* id="confirmReviveButton" *) style="display: none; font-size: 14.47px;" class={myButton} (* onclick="userInput.confirmRevive();" *)>Revive Selected</button>
                      <button (* id="setCardButton" *) style="display: none; font-size: 14.47px;" class={myButton} (* onclick="userInput.setCard();" *)>Set Card</button>
                      <button (* id="skipButton" *) style="display: none; font-size: 14.47px;" class={myButton} (* onclick="userInput.skip();" *)>Skip</button>
                      <button (* id="deployCardButton" *) style="display: none; font-size: 14.47px;" class={myButton} (* onclick="if(phase.selectedPositions[0].length === 1) userInput.deployCard(phase.selectedPositions[0][0]);" *)>Ok</button>
                      <button (* id="skillComponentSelectionButton" *) style="display: none; font-size: 14.47px;" class={myButton} (* onclick="gameInterface.confirmSkillComponentSelection();" *)>Ok</button>
                      <span (* id="cardSearchAreaOtherMore" *) style="display:none;"></span>
                     </span>

                    </div>
                    <div class={col26666}>

                     

                    </div>
                   </div>
                  </div> *)
                  <div style = "padding-top:90%;">
                  <button (* id="cardSearchButton" *) class={myButtonSmall} (* onclick="searchCards();" *) (* style="white-space: pre; font-size: 5.686px;" *) title="Search for cards with specified requirements">Search Cards</button>
                  </div>


                  <div (* id="playerResources" *)>
                   <div class={container8b} style="padding-top:5%;">
                    <div class={container7b}>
                     <div class={container6b}>
                      <div class={container5b}>
                       <div class={container4b}>
                        <div class={container3b}>
                         <div class={container1b}>
                          <div class={col1b}>
                           <section class={stage_thoughts}>
                            <figure (* id="playerThoughtFigure" *) class={ball_thoughts____ball_thoughts_text} (* onclick="thoughtClick();" *) title="Restrict search to cards not having additional schools which you have not selected" (* style="font-size: 4.392px;" *)>
                             <span (* id="playerThoughtSpan" *) class={thought_span} (* style="overflow: hidden; text-align: center; transform: translate(0%, -45%); font-size: 21.96px;" *)></span> 
                            </figure>
                           </section>
                          </div>
                          <div class={col3b} (* style="padding-top:1%; padding-left:7%; padding-right:1%;" *)>
                           <section class={stage}>
                            <button class={plus_button} title="Allocate one thought to your earth knowledge" (* onclick="gameData.clickedIncrementKnowledge(0);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/plus.png"/>
                             </div>
                            </button>
                            <figure (* id="ball1" *) class={ball_earth} style="text-align: center; font-size: 16.47px;" (* onclick="clickEarth();" *) title="Restrict search to earth cards"></figure>
                            <button class={minus_button} title="Cancel allocation of a thought made to your earth knowledge this turn" (* onclick="gameData.clickedDecrementKnowledge(0);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/minus.png"/>
                             </div>
                            </button>
                           </section>
                          </div>
                          <div class={col4b} style="padding-top:1%; padding-left:1%; padding-right:1%;">
                           <section class={stage}>
                            <button class={plus_button} title="Allocate one thought to your fire knowledge" (* onclick="gameData.clickedIncrementKnowledge(1);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/plus.png"/>
                             </div>
                            </button>
                            <figure (* id="ball2" *) class={ball_fire} style="text-align: center; font-size: 16.47px;" (* onclick="clickFire();" *) title="Restrict search to fire cards"></figure>
                            <button class={minus_button} title="Cancel allocation of a thought made to your fire knowledge this turn" (* onclick="gameData.clickedDecrementKnowledge(1);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/minus.png"/>
                             </div>
                            </button>
                           </section>
                          </div>
                          <div class={col5b} style="padding-top:1%; padding-left:1%; padding-right:1%;">
                           <section class={stage}>
                            <button class={plus_button} title="Allocate one thought to your water knowledge" (* onclick="gameData.clickedIncrementKnowledge(2);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/plus.png"/>
                             </div>
                            </button>
                            <figure (* id="ball3" *) class={ball_water} style="text-align: center; font-size: 16.47px;" (* onclick="clickWater();" *) title="Restrict search to water cards"></figure>
                            <button class={minus_button} title="Cancel allocation of a thought made to your water knowledge this turn" (* onclick="gameData.clickedDecrementKnowledge(2);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/minus.png"/>
                             </div>
                            </button>
                           </section>
                          </div>
                          <div class={col6b} style="padding-top:1%; padding-left:1%; padding-right:1%;">
                           <section class={stage}>
                            <button class={plus_button} title="Allocate one thought to your air knowledge" (* onclick="gameData.clickedIncrementKnowledge(3);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/plus.png"/>
                             </div>
                            </button>
                            <figure (* id="ball4" *) class={ball_air} style="text-align: center; font-size: 16.47px;" (* onclick="clickAir();" *) title="Restrict search to air cards"></figure>
                            <button class={minus_button} title="Cancel allocation of a thought made to your air knowledge this turn" (* onclick="gameData.clickedDecrementKnowledge(3);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/minus.png"/>
                             </div>
                            </button>
                           </section>
                          </div>
                          <div class={col7b} style="padding-top:1%; padding-left:1%; padding-right:1%;">
                           <section class={stage}>
                            <button class={plus_button} title="Allocate one thought to your spirit knowledge" (* onclick="gameData.clickedIncrementKnowledge(4);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/plus.png"/>
                             </div>
                            </button>
                            <figure (* id="ball5" *) class={ball_spirit} style="text-align: center; font-size: 16.47px;" (* onclick="clickSpirit();" *) title="Restrict search to spirit cards"></figure>
                            <button class={minus_button} title="Cancel allocation of a thought made to your spirit knowledge this turn" (* onclick="gameData.clickedDecrementKnowledge(4);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/minus.png"/>
                             </div>
                            </button>
                           </section>
                          </div>
                          <div class={col8b} style="padding-top:1%; padding-left:1%; padding-right:1%;">
                           <section class={stage}>
                            <button class={plus_button} title="Allocate one thought to your void knowledge" (* onclick="gameData.clickedIncrementKnowledge(5);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/plus.png"/>
                             </div>
                            </button>
                            <figure (* id="ball6" *) class={ball_void} style="text-align: center; font-size: 16.47px;" (* onclick="clickVoid();" *) title="Restrict search to void cards"></figure>
                            <button class={minus_button} title="Cancel allocation of a thought made to your void knowledge this turn" (* onclick="gameData.clickedDecrementKnowledge(5);" *)>
                             <div class={centralizer}>
                              <img style="width:100%; padding-bottom:100%;" src="./battle.jsp_files/minus.png"/>
                             </div>
                            </button>
                           </section>
                          </div>
                         </div>
                        </div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </div>
                 </div>
                 <div class={outerGraveyard} (* id="cardcardcard" *) (* style="display:none; background-image: -webkit-gradient(linear,
                  left top,
                  left bottom,
                  color-stop(1, #06011F),
                  color-stop(1, #06040F)
                  );
                  background-image: -o-linear-gradient(bottom, #06011F 100%, #06040F 100%);
                  background-image: -moz-linear-gradient(bottom, #06011F 100%, #06040F 100%);
                  background-image: -webkit-linear-gradient(bottom, #06011F 100%, #06040F 100%);
                  background-image: -ms-linear-gradient(bottom, #06011F 100%, #06040F 100%);
                  background-image: linear-gradient(to bottom, #06011F 100%, #06040F 100%);z-index:500; position:relative;" *)>
                  <span class={wrapper} (* id="cardimagecard" *)>
                   <svg style="width:1600px;height:2000px;background:#000;" class={imagWOWURWEBERRORe} ></svg>
                   <div class={positioner}>
                    <div style="padding-top: 125%;">
                     <div class={context_box}>
                      <div class={content}>
                       <div class={container211}>
                        <div class={container111}>
                         <div class={col111}>
                          <img (* id="cardareaimage" *) src="./battle.jsp_files/ViciousSoulReaper.jpg"/> 
                         </div>
                         <div class={col211} style="font-size:15px;text-align:left;">
                          <span (* id="cardname" *) style="color:white;font-size: 7.137px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Schools: </span><span (* id="schools" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Level: </span><span (* id="level" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Hp: </span><span (* id="hp" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Mana: </span><span (* id="mana" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Attack: </span><span (* id="attack" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Defense: </span><span (* id="defense" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Agility: </span><span (* id="agility" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Range: </span><span (* id="range" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          <span class={cardStat} style="color:grey;font-size: 7.686px;">Sp: </span><span (* id="sp" *) class={cardStat} style="color:green;font-size: 7.686px;"></span><br/>
                          (*
                           <span id = "engagementWrapper">
                            <span class = "cardStat" style = "color:grey">Engagement:</span>
                            <span id = "engagement" class={cardStat} style = "color:lightgrey"> Disengaged</span>
                            <br>
                           </span>
                           <span id = "statusWrapper"><span class = "cardStat" style = "color:grey">Status:</span><span id = "status" class = "cardStat" style ="color:green"> None</span><br></span>
                           *)
                         </div>
                        </div>
                       </div>
                       <div (* id="skillArea" *) style="background:#000;">
                        <div class={container3of3with0}>
                         <div class={container2of3with0}>
                          <div class={container1of3with0}>
                           <div class={col1of3with0forskills}>
                            <span class={cardSkill} (* style="color:green;float: left; font-size: 7.686px;" *) (* id="cardSkillType0" *)></span>
                           </div>
                           <div class={col2of3with0forskills}>
                            <span class={cardSkill} style="color:yellow;float: left; font-size: 7.686px;" (* id="cardSkillName0" *)></span>
                           </div>
                           <div class={col3of3with0floatright}>
                            <span class={cardSkill} (* style="color:cyan;float: right; font-size: 7.686px;" *) (* id="cardSkillCost0" *)></span>
                           </div>
                          </div>
                         </div>
                        </div>
                        <div class={cardSkillDescription} style="font-size: 7.686px;"><span style = "color:white" (* id="cardSkillDescription0" *)></span></div>
                        <div class={container3of3with0}>
                         <div class={container2of3with0}>
                          <div class={container1of3with0}>
                           <div class={col1of3with0forskills}>
                            <span class={cardSkill} (* style="color:green;float: left; font-size: 7.686px;" *) (* id="cardSkillType1" *)></span>
                           </div>
                           <div class={col2of3with0forskills}>
                            <span class={cardSkill} style="color:yellow;float: left; font-size: 7.686px;" (* id="cardSkillName1" *)></span>
                           </div>
                           <div class={col3of3with0floatright}>
                            <span class={cardSkill} (* style="color:cyan;float: right; font-size: 7.686px;" *) (* id="cardSkillCost1" *)></span>
                           </div>
                          </div>
                         </div>
                        </div>
                        <div class={cardSkillDescription} style="font-size: 7.686px;"><span style = "color:white" (* id="cardSkillDescription1" *)></span></div>
                        <div class={container3of3with0}>
                         <div class={container2of3with0}>
                          <div class={container1of3with0}>
                           <div class={col1of3with0forskills}>
                            <span class={cardSkill} style="color:green; float: left; font-size: 7.686px;" (* id="cardSkillType2" *)></span>
                           </div>
                           <div class={col2of3with0forskills}>
                            <span class={cardSkill} style="color:yellow; float: left; font-size: 7.686px;" (* id="cardSkillName2" *)></span>
                           </div>
                           <div class={col3of3with0floatright}>
                            <span class={cardSkill} style="color:cyan; float: right; font-size: 7.686px;" (* id="cardSkillCost2" *)></span>
                           </div>
                          </div>
                         </div>
                        </div>
                        <div class={cardSkillDescription} style="font-size: 7.686px;"><span style="color:white" (* id="cardSkillDescription2" *)></span></div>
                        <div class={container3of3with0}>
                         <div class={container2of3with0}>
                          <div class={container1of3with0}>
                           <div class={col1of3with0forskills}>
                            <span class={cardSkill} style="color:green; float: left; font-size: 7.686px;" (* id="cardSkillType3" *)></span>
                           </div>
                           <div class={col2of3with0forskills}>
                            <span class={cardSkill} style="color:yellow;float: left; font-size: 7.686px;" (* id="cardSkillName3" *)></span>
                           </div>
                           <div class={col3of3with0floatright}>
                            <span class={cardSkill} style="color:cyan; float: right; font-size: 7.686px;" (* id="cardSkillCost3" *)></span>
                           </div>
                          </div>
                         </div>
                        </div>
                        <div class={cardSkillDescription} style="font-size: 7.686px;"><span style="color:white;" (* id="cardSkillDescription3" *)></span></div>
                        <div class={container3of3with0}>
                         <div class={container2of3with0}>
                          <div class={container1of3with0}>
                           <div class={col1of3with0forskills}>
                            <span class={cardSkill} style="color:green; float: left; font-size: 7.686px;" (* id="cardSkillType4" *)></span>
                           </div>
                           <div class={col2of3with0forskills}>
                            <span class={cardSkill} style="color:yellow; float: left; font-size: 7.686px;" (* id="cardSkillName4" *)></span>
                           </div>
                           <div class={col3of3with0floatright}>
                            <span class={cardSkill} style="color:cyan; float: right; font-size: 7.686px;" (* id="cardSkillCost4" *)></span>
                           </div>
                          </div>
                         </div>
                        </div>
                        <div class={cardSkillDescription} style="font-size: 7.686px;"><span style="color:white;" (* id="cardSkillDescription4" *)></span></div>
                        <div class={container3of3with0}>
                         <div class={container2of3with0}>
                          <div class={container1of3with0}>
                           <div class={col1of3with0forskills}>
                            <span class={cardSkill} style="color:green; float: left; font-size: 7.686px;" (* id="cardSkillType5" *)></span>
                           </div>
                           <div class={col2of3with0forskills}>
                            <span class={cardSkill} style="color:yellow; float: left; font-size: 7.686px;" (* id="cardSkillName5" *)></span>
                           </div>
                           <div class={col3of3with0floatright}>
                            <span class={cardSkill} style="color:cyan; float: right; font-size: 7.686px;" (* id="cardSkillCost5" *)></span>
                           </div>
                          </div>
                         </div>
                        </div>
                        <div class={cardSkillDescription} (* id="cardSkillDescription5" *) style="font-size: 7.686px;"><span style="color:white;"></span></div>
                        <div class={container3of3with0}>
                         <div class={container2of3with0}>
                          <div class={container1of3with0}>
                           <div class={col1of3with0forskills}>
                            <span class={cardSkill} style="color:green; float: left; font-size: 7.686px;" (* id="cardSkillType6" *)></span>
                           </div>
                           <div class={col2of3with0forskills}>
                            <span class={cardSkill} style="color:yellow; float: left; font-size: 7.686px;" (* id="cardSkillName6" *)></span>
                           </div>
                           <div class={col3of3with0floatright}>
                            <span class={cardSkill} style="color:cyan; float: right; font-size: 7.686px;" (* id="cardSkillCost6" *)></span>
                           </div>
                          </div>
                         </div>
                        </div>
                        <div class={cardSkillDescription} style="font-size: 7.686px;"><span style="color:white;" (* id="cardSkillDescription6" *)></span></div>
                       </div>
                      </div>
                     </div>
                    </div>
                   </div>
                  </span>
                 </div>
                </div>
               </div>
              </div>
             </div>
            </span>
            <div style="width:98%; padding-top:.5%; padding-bottom:.5%; padding-left:.5%; padding-right:1.5%; background-color:#444c4f; display:inline-block;">
             <div style="width:100%; padding:1%; background-color:#222222;">
              <div style="width:100%; background-color:black; padding:0%;">
               <span (* id="lp4-0-wrapper" *) style="display:inline;">
                <div class={lp0}></div>
               </span>
               <span (* id="lp4-1-wrapper" *) style="display:none;">
                <div class={lp1} (* id="lp4-1-1" *)></div>
               </span>
               <span (* id="lp4-2-wrapper" *) style="display:none;">
                <div class={lp2} (* id="lp4-2-1" *)></div>
                <div class={spacing}></div>
                <div class={lp2} (* id="lp4-2-2" *)></div>
               </span>
               <span (* id="lp4-3-wrapper" *) style="display:none;">
                <div class={lp3} (* id="lp4-3-1" *)></div>
                <div class={spacing}></div>
                <div class={lp3} (* id="lp4-3-2" *)></div>
                <div class={spacing}></div>
                <div class={lp3} (* id="lp4-3-3" *)></div>
               </span>
               <div class={spacing}></div>
                <span (* id="lp3-0-wrapper" *) style="display:inline;">
               <div class={lp0}></div>
              </span>
              <span (* id="lp3-1-wrapper" *) style="display:none;">
               <div class={lp1} (* id="lp3-1-1" *)></div>
              </span>
              <span (* id="lp3-2-wrapper" *) style="display:none;">
               <div class={lp2} (* id="lp3-2-1" *)></div>
               <div class={spacing}></div>
               <div class={lp2} (* id="lp3-2-2" *)></div>
              </span>
              <span (* id="lp3-3-wrapper" *) style="display:none;">
               <div class={lp3} (* id="lp3-3-1" *)></div>
               <div class={spacing}></div>
               <div class={lp3} (* id="lp3-3-2" *)></div>
               <div class={spacing}></div>
               <div class={lp3} (* id="lp3-3-3" *)></div>
              </span>
              <div class={spacing}></div>
               <span (* id="lp2-0-wrapper" *) style="display:inline;">
                <div class={lp0}></div>
               </span>
               <span (* id="lp2-1-wrapper" *) style="display:none;">
                <div class={lp1} (* id="lp2-1-1" *)></div>
               </span>
              <span (* id="lp2-2-wrapper" *) style="display:none;">
               <div class={lp2} (* id="lp2-2-1" *)></div>
               <div class={spacing}></div>
               <div class={lp2} (* id="lp2-2-2" *)></div>
              </span>
              <span (* id="lp2-3-wrapper" *) style="display:none;">
               <div class={lp3} (* id="lp2-3-1" *)></div>
               <div class={spacing}></div>
               <div class={lp3} (* id="lp2-3-2" *)></div>
               <div class={spacing}></div>
               <div class={lp3} (* id="lp2-3-3" *)></div>
              </span>
              <div class={spacing}></div>
              <span (* id="lp1-0-wrapper" *) style="display:inline;">
               <div class={lp0}></div>
              </span>
              <span (* id="lp1-1-wrapper" *) style="display:none;">
               <div class={lp1} (* id="lp1-1-1" *)></div>
              </span>
              <span (* id="lp1-2-wrapper" *) style="display:none;">
               <div class={lp2} (* id="lp1-2-1" *)></div>
               <div class={spacing}></div>
               <div class={lp2} (* id="lp1-2-2" *)></div>
              </span>
              <span (* id="lp1-3-wrapper" *) style="display:none;">
               <div class={lp3} (* id="lp1-3-1" *)></div>
               <div class={spacing}></div>
               <div class={lp3} (* id="lp1-3-2" *)></div>
               <div class={spacing}></div>
               <div class={lp3} (* id="lp1-3-3" *)></div>
              </span>
              <div class={spacing}></div>
               <span (* id="lp0-0-wrapper" *) style="display:inline;">
                <div class={lp0}></div>
               </span>
               <span (* id="lp0-1-wrapper" *) style="display:none;">
                <div class={lp1} (* id="lp0-1-1" *)></div>
               </span>
               <span (* id="lp0-2-wrapper" *) style="display:none;">
                <div class={lp2} (* id="lp0-2-1" *)></div>
                <div class={spacing}></div>
                <div class={lp2} (* id="lp0-2-2" *)></div>
               </span>
               <span (* id="lp0-3-wrapper" *) style="display:none;">
                <div class={lp3} (* id="lp0-3-1" *)></div>
                <div class={spacing}></div>
                <div class={lp3} (* id="lp0-3-2" *)></div>
                <div class={spacing}></div>
                <div class={lp3} (* id="lp0-3-3" *)></div>
               </span>
              </div>
             </div>
           </div>
           <span (* id="nhhh" *) (* style="float:left;background: rgba(0, 0, 0, 1.0); width:100%; padding:1%; border-radius:10%;" *)>
            <div class={container5} (* style="background: rgba(200, 230, 255, 0.4); padding-bottom:1%; border-radius:5%; width:98%;" *)>
             <div class={container4}>
              <div class={container3}>
               <div class={container2}>
                <div class={container1}>
                 <div class={col1}>
                  <div (* onclick="phase.clickPlayerSoul(4)" *) (* id="playerSoul4" *) title="The fifth and last position in your soul cards. The card placed here will be the last to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points. If lose any soul points beyond this, you will lose the round.">
                   <div (* id="playerSoulContainer4" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                    <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulFace4" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img (* id="playerSoulImage4" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulBack4" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                  </div>
                 </div>
                 <div class={col2}>
                  <div (* onclick="phase.clickPlayerSoul(3)" *) (* id="playerSoul3" *) title="The fourth position in your soul cards. The card placed here will be the fourth to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                   <div (* id="playerSoulContainer3" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                    <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulFace3" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img (* id="playerSoulImage3" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulBack3" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                  </div>
                 </div>
                 <div class={col3}>
                  <div (* onclick="phase.clickPlayerSoul(2)" *) (* id="playerSoul2" *) title="The third position in your soul cards. The card placed here will be the third to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                   <div (* id="playerSoulContainer2" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                    <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulFace2" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img (* id="playerSoulImage2" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulBack2" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                  </div>
                 </div>
                 <div class={col4}>
                  <div (* onclick="phase.clickPlayerSoul(1)" *) (* id="playerSoul1" *) title="The second position in your soul cards. The card placed here will be the second to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                   <div (* id="playerSoulContainer1" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                    <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulFace1" *) class={face____front____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img (* id="playerSoulImage1" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulBack1" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                  </div>
                 </div>
                 <div class={col5}>
                  <div (* onclick="phase.clickPlayerSoul(0)" *) (* id="playerSoul0" *) title="The first position in your soul cards. The card placed here will be the first to absorb soul damage, and its soul skill will trigger when it reaches 0 soul points.">
                   <div (* id="playerSoulContainer0" *) class={unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;">
                    <img src="https://dl.dropbox.com/s/ic01iftgmkjw9ru/soul.png?dl=0" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulFace0" *) class={face____front____card____unselected} (* style="border-radius:10%; margin-top:4%; margin-bottom:0%; display:none;" *)>
                    <img (* id="playerSoulImage0" *) src="./battle.jsp_files/ApocalypseDragonDisplay.jpg" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                   <div (* id="playerSoulBack0" *) class={backside____backRotate____back____card____unselected} style="border-radius:10%; margin-top:4%; margin-bottom:0%;  display:none;">
                    <img src="./battle.jsp_files/cardback.png" style="width:80%; padding:10%; border-radius:10%;"/>
                   </div>
                  </div>
                 </div>
                </div>
               </div>
              </div>
             </div>
            </div>
           </span>
(*
           <div class={container2of2with0}>
            <div class={container1of2with0}>
             <div class={col1of2with0}>
              <button class={myButtonResign} (* style="width: 100%; padding-top: 10%; padding-bottom: 10%; font-size: 5.686px;" *) (* onclick="userInput.resignRound();" *)>Resign Round</button>
             </div>
             <div class={col2of2with0}>
              <button class={myButtonResign} (* style="width: 100%; padding-top: 10%; padding-bottom: 10%; font-size: 5.686px;" *) (* onclick="userInput.resignGame();" *)>Resign Game</button>
             </div>
            </div>
           </div>
           <div (* style="display:none" *)>
            <button class={myButtonResign} (* style="width: 100%; padding-top: 10%; padding-bottom: 10%; font-size: 5.686px;" *)>Return to Lobby</button>
           </div>
*)
          </div>
          <div class={col3of3with2big}>
           <div style =	"position: relative;">
            <svg style = "width:2000px;height:4700px;" (*class="imageMessages"*) class = {imagWOWURWEBERRORe}></svg> 
            <div class={positionerMessages} style = "position:absolute;top:0px;width:100%; background-color:red;height:100%;">
             <div style = "width:100%; background-color:blue; height:18%;">
              <div style = "width:50%; height:100%; background-color:orange;float:left;">
               <div class={clockDisplay} title="When your opponent runs out of time, they lose the round. Time is allocated separately at the beginning of the first and second rounds." (* id="enemyClockDisplay" *) style="font-size: 10px;">
                        15:00
               </div>
               <div class={clockDisplay} title="When you run out of time, you lose the round. Time is allocated separately at the beginning of the first and second rounds." (* id="playerClockDisplay" *) style="font-size: 10px;">
                  15:00
               </div>
              </div>
              <div style = "width:50%; height:100%; background-color:yellow;float:left;">
                <button class={myButtonResign} style="float:left; width:100%; height: 100%; font-size: 5.686px;" (* onclick="userInput.resignRound();" *)>Resign
                </button>
               </div>
              </div>
              <div style = "width:100%; background-color:pink; height:82%;clear:both;">
               <div (*readonly=""*) (*name="comments"*) (* id="messageArea" *) style="height:100%; font-family:cursive; font-size:12px; text-align:left;border:groove #AAAAAA; resize:none; background-color:black; color:white; overflow-y:scroll; white-space:pre-wrap;">
               </div>
              </div>
             </div>
            </div>
           </div>
          </div>
         </div>
        </div>
       </div>
      </div>
     </div>
    </span>
   </div>
  </div>
 </body>
</xml>
