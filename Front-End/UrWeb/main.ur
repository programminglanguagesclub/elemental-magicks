

style navMenu
style wrapper
style metal
style linear
style content
style bodyArea
style footerClass



(*working on getting css for these*)

style image
style positioner
style contentbox
style idc





















fun generate_token () : transaction string =
 random_number <- rand;
 return (show (random_number))

fun gui () : transaction page = return <xml><body></body></xml>


fun counter () =
  n1 <- UrwebFFI.counter ();
 (* n2 <- UrwebFFI.counter ();*)
  return <xml><body>
   (* {[n1]}, {[n2]}*)
   {[n1]} (* "really broken" *)
  </body></xml>



fun stress_test (x : int) : transaction page =
  if x = 0
   then
    return <xml><body> All Done!! </body> </xml>
   else
    _ <- UrwebFFI.counter ();
    stress_test (x-1)



(* {[Js.jsTest 100000]} *)

fun barbar (x : int) : int = x + 7

fun test () : transaction page =
 return <xml><body><button value = "Click me!" onclick ={fn _ => alert(show ( Js.finishTest (Js.realTest barbar)))}/></body></xml>


(* stress_test 100000 *) (*return <xml><body><button value="Click me!" onclick={fn _ => alert (show (Js.jsTest 100000))}/></body></xml>*)


fun main () : transaction page = return <xml><body>
  {[UrwebFFI.hello ()]}<br/>
  {[UrwebFFI.important "X"]}<br/>
  <form> <submit value="Counter fun!" action={counter}/> </form>
</body></xml>





(*


(* leaving out microdata and styles for now (have to go into apache2 stuff or urp or something to set rewrites for styles*)
fun credits () : transaction page = return <xml>
 <head><title>Credits</title><link rel="stylesheet" type="text/css" href="http://159.203.219.123/credits.css"/><link rel="stylesheet" type="text/css" href="159.203.219.123/home.css"/></head>
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



*)




(*Not visible yet*)
fun home () : transaction page = return <xml>
<head>
 <title>Elemental Magicks</title>
 <link rel="stylesheet" type="text/css" href="http://159.203.219.123/home.css"/>
</head>
<body><div class = {wrapper}>
 <header><h1>Elemental Magicks</h1></header>
 <nav>
  <div class = {navMenu}><a href = "http://159.203.219.123">Introduction</a></div>
  <div class = {navMenu}><a href = "http://159.203.219.123">Tutorial</a></div>
  <div class = {navMenu}><a href = "http://159.203.219.123">Rules</a></div>
  <div class = {navMenu}><a href = "http://159.203.219.123">Cards</a></div>
  <div class = {navMenu}><a href = "http://159.203.219.123">Credits</a></div>
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



















fun battle () : transaction page = return <xml>
 <head>
 </head>
 <body>
  <div>
   <div>
    <span class={wrapper}>
(*   <svg class={image}></svg>*)
     <div class={positioner}>
      <div>
       <div class={contentbox}>
        <div class={content}>
         (*<canvas class={idc}></canvas>*)
        </div>
       </div>
      </div>
     </div>
    </span>
   </div>
  </div>
 </body>
</xml>




