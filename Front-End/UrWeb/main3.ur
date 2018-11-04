
style unselected


(*from standard lib*)
val rev = fn [a] =>
             let
                 fun rev' acc (ls : list a) =
                     case ls of
                         [] => acc
                       | x :: ls => rev' (x :: acc) ls
             in
                 rev' []
             end

(*---------------------------------------------------------------------------*)
fun guh [goo ::: Type] (x : goo) = x

(*---------------------------------------------------------------------------*)
fun replicate
 (i : int)
 (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]))
 : list (xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) =

 let 
  fun replicate'
   (i : int)
   (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]))
   (acc : list (xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])))
   : list (xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) =
   case i of
    0 => rev acc
    | _ => replicate' (i - 1) x (x::acc)
 in
  replicate' i x []
 end
(*---------------------------------------------------------------------------*)
fun wrapList
  (x : list (xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])))
  : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) =
 
 case x of
  [] => <xml></xml>
  | (y::ys) => <xml>{y} {wrapList ys}</xml>
(*---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------*)
fun replicateAndWrapWithDiv
 (i : int)
 (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]))
 (y : css_class)
 : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) =

 <xml><div class={y}>{wrapList (replicate i x)}</div></xml>
(*---------------------------------------------------------------------------*)
fun replicateAndWrapWithSpan
 (i : int)
 (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]))
 (y : css_class)
 : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) =

 <xml><span class={y}>{wrapList (replicate i x)}</span></xml>
(*---------------------------------------------------------------------------*)
val getBlah : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
<div></div>
</xml>


val fooBar : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
  <img style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
 </div>
</xml>

val manyFoos = wrapList (replicate 10 fooBar)



fun wrap (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div class={unselected}> {x} </div>
</xml>

fun wrapWith (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) (y : css_class) : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([]) = <xml>
 <div class={y}> {x} </div>
</xml>



val manyClasses : list css_class = unselected::unselected::unselected::unselected::[]

(*
fun argle (l : list css_class) (x : xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) : list (xml ([Dyn = (), MakeForm = (), Body = ()]) ([]) ([])) =
 map (wrapWith x) l
*)

fun battle (x : string) : transaction page = return <xml>
 <head></head><body><button onclick = {fn _ => alert(x)}>CLICK!</button>{fooBar}{wrapWith getBlah unselected}</body></xml>


fun main () : transaction page = return <xml><head></head><body><button onclick = {fn _ => alert("hello")}>CLICK!</button>{fooBar}{wrapWith getBlah unselected}</body></xml>


(*


 <div (* id="enemygraveyard0" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="0" *) class={enemyGraveyardImage} (* id="enemygraveyardImage0" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
                         <div (* id="enemygraveyard5" *) style="visibility:hidden; border-radius:10%;margin-top:0%; margin-bottom:4%;" class={unselected}>
                          <img (* index="5" *) class={enemyGraveyardImage} (* id="enemygraveyardImage5" *) style="width:80%; padding:10%; border-radius:10%;" src="./battle.jsp_files/ApocalypseDragonDisplay.jpg"/>
                         </div>
*)
