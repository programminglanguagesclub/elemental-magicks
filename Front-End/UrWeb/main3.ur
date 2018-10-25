style unselected


val getBlah : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
<div></div>
</xml>


val fooBar : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;">
  <img style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
 </div>
</xml>



fun wrap (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div class={unselected}> {x} </div>
</xml>

fun wrapWith (y : css_class) (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div class={y}> {x} </div>
</xml>


fun main () : transaction page = return <xml><head></head><body>{fooBar}</body></xml>
