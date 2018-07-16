module Main.Revival_phase
import Data.Vect
import Data.Fin
import Base.Bounded
import Base.Preliminaries
import Base.Objects_basic
import Base.Card
import Base.Player
import Main.Game
import Main.Serverupdates
import Base.Clientupdates
import Base.Skill_dsl_data
%access public export
%default total

-------------------------------------------------------------------------------
getNumberOfSchools : BasicFieldedMonster -> Nat
getNumberOfSchools monster with (schools monster)
 | NoSchools      = 0
 | OneSchool _    = 1
 | TwoSchools _ _ = 2
-------------------------------------------------------------------------------


{-


getReviveCost : (toRevive : List FieldedMonster) -> Nat -- refactor
getReviveCost toRevive =
 foldl (\n => \m => (n + (getNumberOfSchools (basic m)))) 0 toRevive
-------------------------------------------------------------------------------






-- make more verified

{-I might want to move these to the graveyard rather than simply removing them from the hand-}
-------------------------------------------------------------------------------
_removeMonsterFromHandByPermanentId :
 (acc : List Card) ->
 (hand : List Card) ->
 (id : Nat) ->
 (Maybe UnfieldedMonster, List Card)

_removeMonsterFromHandByPermanentId _ [] _ = (Nothing,[])
_removeMonsterFromHandByPermanentId acc ((SpellCard _)::xs) id =
 _removeMonsterFromHandByPermanentId acc xs id
_removeMonsterFromHandByPermanentId acc ((MonsterCard m)::xs) permanentId =
 if (id (basic m)) == permanentId
  then
   (Just m,acc ++ xs)
  else
   _removeMonsterFromHandByPermanentId (acc ++ [MonsterCard m]) xs permanentId

{-could optimize this to use :: instead of ++ and then reverse at the end-}

-------------------------------------------------------------------------------
removeMonsterFromHandByPermanentId :
 (hand : List Card) ->
 (id : Nat) ->
 (Maybe UnfieldedMonster, List Card)

removeMonsterFromHandByPermanentId = _removeMonsterFromHandByPermanentId []
-------------------------------------------------------------------------------
moveMonsterFromHandToGraveyardByPermanentId :
 (hand : List Card) ->
 (graveyard : List Card) ->
 (id : Nat) ->
 Maybe (List Card, List Card)

moveMonsterFromHandToGraveyardByPermanentId hand graveyard id =
 case removeMonsterFromHandByPermanentId hand id of
  (Nothing, hand') => Nothing
  (Just m, hand') => Just (hand', (graveyard ++ [MonsterCard m]))
-}



-------------------------------------------------------------------------------
reviveMonster : FieldedMonster -> FieldedMonster
reviveMonster fieldedMonster = record {basic $= revive} fieldedMonster
-- objects basic defines revive basic fielded monster...

{-
--thoughtsResource : Bounded 0 Preliminaries.absoluteUpperBound

_revive :
 Vect 9 Bool ->
 Vect 9 (Maybe FieldedMonster) ->
 (thoughts : Bounded 0 Preliminaries.absoluteUpperBound) ->
 List Card ->
 List Card ->
 Maybe (Vect 9 (Maybe FieldedMonster),Bounded 0 Preliminaries.absoluteUpperBound,List Card,List Card)

_revive positions board thoughts hand graveyard =
 let zipped = zipWith reviveSelectedMonsters positions board in
 let revivedMonsters = justRevivedMonsters (toList zipped) in
 (case moveMonsterFromHandToGraveyardByPermanentId hand graveyard ?hole of
      Nothing => Nothing
      Just (hand', graveyard') => Just (board, thoughts, hand, graveyard)) where
  -----------------------------------------------------------------------------
  reviveSelectedMonsters : Bool -> Maybe FieldedMonster -> Maybe FieldedMonster
  reviveSelectedMonsters True (Just m) = Just (reviveMonster m)
  reviveSelectedMonsters _ _ = Nothing
  -----------------------------------------------------------------------------
  justRevivedMonsters : List (Maybe FieldedMonster) -> List FieldedMonster
  justRevivedMonsters [] = []
  justRevivedMonsters (Nothing::xs) = justRevivedMonsters xs
  justRevivedMonsters ((Just m)::xs) = [m] ++ (justRevivedMonsters xs)
  -----------------------------------------------------------------------------
  {-removeRevivedCards : List Monster -> List Card -> List Card -> (List Card, List Card)
  removeRevivedCards [] hand graveyard = (hand, graveyard)
  removeRevivedCards (x::xs) hand graveyard = ?h
  -}
  -}
-------------------------------------------------------------------------------
removeMonsterFromHandByName :
 (hand : List Card) ->
 (name : String) ->
 Maybe (Card, List Card)

removeMonsterFromHandByName [] _ = Nothing
removeMonsterFromHandByName ((SpellCard _)::hs) n =
 removeMonsterFromHandByName hs n
removeMonsterFromHandByName ((MonsterCard unfieldedMonster)::hs) n =
 case (name $ basic $ unfieldedMonster) == n of
  True => Just (MonsterCard unfieldedMonster, hs)
  False =>
   case removeMonsterFromHandByName hs n of
    Nothing => Nothing
    Just (card, hand') => Just (card, (MonsterCard unfieldedMonster)::hand')
  

      {-

      data Card
         = SpellCard Spell
          | MonsterCard UnfieldedMonster

          -}
-------------------------------------------------------------------------------
revive : -- ignoring the death queue for now, although that also must be passed..

 (positions : Vect n Bool) ->
 (board : Vect n (Maybe FieldedMonster)) ->
 (thoughts : Nat) ->
 (hand : List Card) ->
 Either String (Vect n (Maybe FieldedMonster), Nat, List Card, List Card) -- hand and then grave at the end.

revive [] [] thoughts hand = Right ([],thoughts, hand, [])
revive (False::ps) (b::bs) thoughts hand =
 case revive ps bs thoughts hand of
  Left error => Left error
  Right (board', thoughts', hand', graveyard') => Right (b::board', thoughts', hand', graveyard')
revive (True::ps) (Nothing::bs) _ _ =
 Left "You cannot revive empty locations"
revive (True::ps) ((Just fieldedMonster)::bs) thoughts hand =
 let reviveCost = getNumberOfSchools $ basic fieldedMonster in
 if thoughts >= reviveCost
  then
   case removeMonsterFromHandByName hand $ name $ basic fieldedMonster of
    Nothing =>
     Left "You do not have the replacement cards in your hand to afford this revival."
    Just (copy, hand') =>
     case revive ps bs (minus thoughts reviveCost) hand' of
      Left error => Left error
      Right (board', thoughts', hand'', graveyard') =>
       Right ((Just (reviveMonster fieldedMonster))::board', thoughts', hand'', copy::graveyard')
  else
   Left "You do not have sufficient thoughts to afford this revival."




{-
revive positions player = ?hole -- must figure out structure of board....
                          -}


{-
 case _revive positions (board player) (thoughtsResource player) (hand player) (graveyard player) of
  Nothing => Nothing
  Just (board', thoughts', hand', graveyard') =>
   Just (record {board = board', thoughts = thoughts', hand = hand', graveyard = graveyard'} player)
-}

{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}

transformRevivalPhase :
 (playerToUpdate : Player) ->
 (deathQueueByTemporaryId : List Nat) ->
 (serverUpdate : ServerUpdate) ->
 Either
  String
  (Player,List Nat, List ClientUpdate) -- also return an updated deathQueue
 
 
