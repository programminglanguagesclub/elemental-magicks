


style navMenu
style wrapper
style metal
style linear
style content
style bodyArea
style footerClass









fun ask_idris (message : string) : string = "a"







fun counter () =
  n1 <- Lib.counter ();
 (* n2 <- Lib.counter ();*)
  return <xml><body>
   (* {[n1]}, {[n2]}*)
    {[n1]}
  </body></xml>




fun main () : transaction page = return <xml><body>
  {[Lib.hello ()]}<br/>
  {[Lib.important "X"]}<br/>
  <form> <submit value="Counter fun!" action={counter}/> </form>
</body></xml>








(* leaving out microdata and styles for now (have to go into apache2 stuff or urp or something to set rewrites for styles*)
fun credits () : transaction page = return <xml>
 <head><title>Credits</title><link rel="stylesheet" type="text/css" href="http://elementalmagicks.ml/credits.css"/><link rel="stylesheet" type="text/css" href="http://elementalmagicks.ml/home.css"/></head>
 <body><div class = {wrapper}>
  <header><h1>Elemental Magicks Credits</h1></header>
  <nav>
   <hr style = "clear:both;"/>
  </nav>
  <div class={content}><div class={bodyArea}>
   <h2>The following are the artists who have made Elemental Magicks possible, together with their respective artwork. The artwork has been modified for the game.</h2>
   <span>
    <div>
     <span><a href="http://bobbie-the-jean.deviantart.com/"><></></span>
    </div>
   </span>
  </div></div>
</div></body></xml>








(*Not visible yet*)
fun home () : transaction page = return <xml>
<head>
 <title>Elemental Magicks</title>
 <link rel="stylesheet" type="text/css" href="http://elementalmagicks.ml/home.css"/>
</head>
<body><div class = {wrapper}>
 <header><h1>Elemental Magicks</h1></header>
 <nav>
  <div class = {navMenu}><a href = "http://elementalmagicks.ml">Introduction</a></div>
  <div class = {navMenu}><a href = "http://elementalmagicks.ml">Tutorial</a></div>
  <div class = {navMenu}><a href = "http://elementalmagicks.ml">Rules</a></div>
  <div class = {navMenu}><a href = "http://elementalmagicks.ml">Cards</a></div>
  <div class = {navMenu}><a href = "http://elementalmagicks.ml">Credits</a></div>
  <hr style = "clear:both;"/>
 </nav>
 <div style = "text-align:center;"><h2>Elemental Magicks is the only game in the world in which outcomes are determined entirely by objective quality of play</h2></div>
 <div class={content}>
  <div class={bodyArea}><button class={classes metal linear}><a style = "color:black;text-decoration: none;" (*href = ""*)>Enter Game</a></button></div>
 </div>
 <footer class = {footerClass}>
  <h2>Elemental Magicks is currently under development</h2>
  <h3>This game currently requires Google Chrome</h3>
 </footer>
</div></body></xml>
