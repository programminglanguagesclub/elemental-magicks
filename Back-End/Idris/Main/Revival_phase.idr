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

removeFromDeathQueue : Nat -> List Nat -> List Nat
removeFromDeathQueue _ [] = [] -- GAME LOGIC ERROR: card was revived but wasn't in the death queue
removeFromDeathQueue n (x::xs) =
 case x == n of
  True => xs
  False => x :: (removeFromDeathQueue n xs)


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
-------------------------------------------------------------------------------
revive :
 (positions : Vect n Bool) ->
 (board : Vect n (Maybe FieldedMonster)) ->
 (thoughts : Nat) ->
 (deathQueue : List Nat) ->
 (hand : List Card) ->
 Either String (Vect n (Maybe FieldedMonster), Nat, List Nat, List Card, List Card) -- hand and then grave at the end.

revive [] [] thoughts deathQueue hand = Right ([],thoughts, deathQueue, hand, [])
revive (False::ps) (b::bs) thoughts deathQueue hand =
 case revive ps bs thoughts deathQueue hand of
  Left error => Left error
  Right (board', thoughts', deathQueue', hand', graveyard') => Right (b::board', thoughts', deathQueue', hand', graveyard')
revive (True::ps) (Nothing::bs) _ _ _ =
 Left "You cannot revive empty locations"
revive (True::ps) ((Just fieldedMonster)::bs) thoughts deathQueue hand =
 let reviveCost = getNumberOfSchools $ basic fieldedMonster in
 if thoughts >= reviveCost
  then
   case removeMonsterFromHandByName hand $ name $ basic fieldedMonster of
    Nothing =>
     Left "You do not have the replacement cards in your hand to afford this revival."
    Just (copy, hand') =>
     case revive ps bs (minus thoughts reviveCost) (removeFromDeathQueue (id $ basic fieldedMonster) deathQueue) hand' of
      Left error => Left error
      Right (board', thoughts', deathQueue', hand'', graveyard') =>
       Right ((Just (reviveMonster fieldedMonster))::board', thoughts', deathQueue', hand'', graveyard' ++ [copy])
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

foo : Bool -> Selection
foo True = Selected
foo False = Unselected

transformRevivalPhase :
 (playerToUpdate : Player) ->
 (whichPlayer : WhichPlayer) ->
 (deathQueue : List Nat) ->
 (serverUpdate : ServerUpdate) ->
 Either
  String
  (Player,List Nat, List ClientUpdate) -- also return an updated deathQueue
 

-- The Client is required to remove thoughts and move copies from hand to graveyard in the right order, etc.
-- I am only telling the client to perform the revival.
transformRevivalPhase player actor deathQueue serverUpdate =
 case serverUpdate of
  Revive positions =>
   case revive positions (flattenBoard $ board player) (fromIntegerNat $ extractBounded $ thoughtsResource player) deathQueue (hand player) of
    Left error => Left error
    Right (board', thoughts', deathQueue', hand', additionalGraveyard') =>
     Right
      (record {
        board = unflattenBoard board',
        hand = hand',
        graveyard = (graveyard player) ++ additionalGraveyard'
       } player,
       deathQueue',
       [Revive (map foo positions) actor])
  _ => Left "You can only revive cards in the revival phase"

-------------------------------------------------------------------------------
-- this is useful to know if we should wait for player revival input or not.

canReviveAnything : (player : Player) -> Bool

canReviveAnything player = ?hole







