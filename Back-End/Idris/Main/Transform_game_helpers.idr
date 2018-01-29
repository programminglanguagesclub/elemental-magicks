module Main.Transform_game_helpers
import Data.Vect
import Base.Bounded
import Base.Preliminaries
import Base.Objects_basic
import Base.Player
import Base.Card
import Main.Game
import Base.Clientupdates
%access public export
%default total

-------------------------------------------------------------------------------
schoolsHighEnoughToPlayCard :
 Player ->
 Card ->
 Bool

schoolsHighEnoughToPlayCard player (SpellCard card) =
 geq (index (school $ basic card) $ knowledge player) (level $ basic card)

schoolsHighEnoughToPlayCard player (MonsterCard card) with (schools (basic card))
 | NoSchools = True
 | OneSchool s =
  geq (index s $ knowledge player) (snd $ snd $ level $ basic card)
 | TwoSchools s1 s2 =
  geq (index s1 $ knowledge player) (snd $ snd $ level $ basic card) &&
  geq (index s2 $ knowledge player) (snd $ snd $ level $ basic card)
-------------------------------------------------------------------------------



{-

  findWithIndexPreferentiallyFrom :
   DecEq a =>
   (p : a -> Bool) ->
   (begin : Fin (S n)) ->
   (v1 : Vect (S n) a) ->
   Either (find p v1 = Nothing) (DPair (Fin (S n), a) (\(i1,e1) => (Vect.index i1 v1 = e1, ((i2 : Fin (S n)) -> (So (p (Vect.index i2 v1))) -> leq2 $ compare (computeSearchIndex begin i1) (computeSearchIndex begin i2) = True))))


-}

-------------------------------------------------------------------------------
--something : Maybe a -> Bool
--something (Just x) = True
--something Nothing = False

-------------------------------------------------------------------------------
reindex :
 Ord b =>
 Vect n (Maybe a) ->
 (engagementPriority : (a, Fin n) -> b) ->
 DPair                                            -- DATA
  (Vect n (Maybe a, Fin n))                       -- Fin n indexes old vector
--------------------------------------------------
  (\v => (                                        -- SPECIFICATION
    (i : Fin n) ->
    (j : Fin n) -> (
     index i v = (Just iElement, iOriginalIndex), -- given two actual elements
     index j v = (Just jElement, jOriginalIndex),
     (prf : compare i j = LT) ->                  -- if i < j in result vector
      compare
       (engagementPriority (iElement, iOriginalIndex))
       (engagementPriority (jElement, jOriginalIndex)) = GT ->
      Void                                        -- cards @ i <= j in priority
  )))


-------------------------------------------------------------------------------

{-For now, completely ignore the possibility of the user using skills! :D -}

_engagementOnMove :
 (playerABoard : Vect 9 (Maybe Monster)) ->
 (playerBBoard : Vect 9 (Maybe Monster)) ->
 (initiative : WhichPlayer) ->
 (WhichPlayer, Fin 9)

_engagementOnMove playerABoard playerBBoard initiative = ?hole
 --let playerAUnit = findWithIndexPreferentiallyFrom ?hole ?hole ?hole in ?hole
-------------------------------------------------------------------------------
engagementOnMove :
 (game : Game) ->
 (player : Player) ->
 (opponent : Player) ->
 (Bool, Fin 9)


-------------------------------------------------------------------------------



{-could return a maybe nat, where nothing indicates an error, but I'll trust the ability to not have it the engagement phase if there's nothing next to move-}


-------------------------------------------------------------------------------

_getEnemyStuffInFront : (defenderBoard) -> (row : Fin 3) -> Nat
_getFriendlyStuffInFront : (attackerBoard : Vect 9 (Maybe Monster)) -> (attackerSquare : Fin 9) -> Nat 
inRangeRow : (attackerBoard : Vect 9 (Maybe Monster)) -> (defenderBoard : Vect 9 (Maybe Monster)) -> (attackerSquare : Fin 9) -> (row : Fin 3) -> Maybe Bool
inRangeRow attackerBoard defenderBoard attackerSquare row with (index attackerSquare attackerBoard)
 | Nothing = Nothing
 | Just monster with (aliveness (basic monster))
  | DeadFresh = Nothing
  | DeadStale = Nothing
  | Alive with (range (basic monster))
   | (temporaryRange,_,_) = if gt (fromIntegerNat (extractBounded temporaryRange)) ((_getFriendlyStuffInFront attackerBoard attackerSquare) + (_getEnemyStuffInFront defenderBoard row)) then Just True else Just False

moveUnit : (moveFrom : Fin 9) -> (moveTo : Fin 9) -> (board : Vect 9 (Maybe Monster)) -> Vect 9 (Maybe Monster) {-this actually does a swap-}
moveUnit moveFrom moveTo board = let to = index moveTo board in replaceAt moveFrom to (replaceAt moveTo (index moveFrom board) board)

restUnit : (location : Fin 9) -> Game -> WhichPlayer -> (Game,List ClientUpdate)

_getHandCards : (hand : List Card) -> (acc : MultiTree Nat) -> MultiTree Nat
_getHandCards [] acc = acc
_getHandCards (card::cards) acc with (card)
 |MonsterCard m      = _getHandCards cards (insert acc (id (basic m)))
 |SpellCard s        = _getHandCards cards acc
getHandCards : (hand : List Card) -> MultiTree Nat
getHandCards hand = _getHandCards hand Leaf

