module Main.Draw_phase
import Data.Vect
import Base.Preliminaries
import Main.Game
import Base.BoundedList
import Base.Player
import Base.Card
import Base.Clientupdates
import Main.Serverupdates
import Cards.Card_list
import Base.Phase
import Base.Skill_dsl_data

%access public export
%default total

data CardDraw' = HA | HB | SA | SB

drawSequence' : List CardDraw'
drawSequence' = [
 HA, HB, HB, HA,   HB, HA, HA, HB,   HA, HB, SB, SA,
 HB, HA, HA, HB,   HA, HB, HB, HA,   HB, HA, SA, SB,
 HA, HB, HB, HA,   HB, HA, HA, HB,   HA, HB, SB, SA,
 HB, HA, HA, HB,   HA, HB, HB, HA,   HB, HA, SA, SB,
 HA, HB, HB, HA,   HB, HA, HA, HB,   HA, HB, SB, SA ]

data CardDraw = Hand | Soul

computeCardDraw : CardDraw' -> (CardDraw, WhichPlayer)
computeCardDraw HA = (Hand, PlayerA)
computeCardDraw HB = (Hand, PlayerB)
computeCardDraw SA = (Soul, PlayerA)
computeCardDraw SB = (Soul, PlayerB)


drawSequence : List (CardDraw, WhichPlayer)
drawSequence = map computeCardDraw drawSequence'

yourTurnToDraw : (CardDraw, WhichPlayer) -> WhichPlayer -> Bool
yourTurnToDraw (_,PlayerA) PlayerA = True
yourTurnToDraw (_,PlayerB) PlayerB = True
yourTurnToDraw _ _ = False

{-
correctLength : List.length drawSequence = 60
correctLength = Refl
-}

yourHand : String
yourHand = "Select a card to add to your hand."
opponentHand : String
opponentHand = "Wait for your opponent to add a card to their hand."
yourSoul : String
yourSoul = "Select a position in your soul and a card to add to your soul."
opponentSoul : String
opponentSoul = "Wait for your opponent to add a card to their soul."



serializeSequence : (CardDraw, WhichPlayer) -> WhichPlayer -> String
serializeSequence (Hand, PlayerA) PlayerA = yourHand
serializeSequence (Hand, PlayerA) PlayerB = opponentHand
serializeSequence (Soul, PlayerA) PlayerA = yourSoul
serializeSequence (Soul, PlayerA) PlayerB = opponentSoul
serializeSequence (Hand, PlayerB) PlayerA = opponentHand
serializeSequence (Hand, PlayerB) PlayerB = yourHand
serializeSequence (Soul, PlayerB) PlayerA = opponentSoul
serializeSequence (Soul, PlayerB) PlayerB = yourSoul


{- move to player -}
namespace wrong
 getActor : WhichPlayer -> Player -> Player -> Player
 getActor PlayerA playerA _ = playerA
 getActor PlayerB _ playerB = playerB

getNonactor : WhichPlayer -> Player -> Player -> Player
getNonactor actor playerA playerB = getActor actor playerB playerA

getActorNonactor : WhichPlayer -> Player -> Player -> (Player,Player)
getActorNonactor actor playerA playerB = (getActor actor playerA playerB, getNonactor actor playerA playerB)

getPlayerAPlayerB : WhichPlayer -> (theActor : Player) -> (theNonactor : Player) -> (Player,Player)
getPlayerAPlayerB PlayerA theActor theNonactor = (theActor, theNonactor)
getPlayerAPlayerB PlayerB theActor theNonactor = (theNonactor, theActor)



getCardDraw : DrawPlayer -> DrawPlayer -> Maybe (CardDraw, WhichPlayer)
getCardDraw playerA playerB = ?hole {-List.index' (List.length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) drawSequence-}



 {-let cardsDrawn = length (doIt (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) in
 case strengthen cardsDrawn of
      Left sameCardsDrawn => Nothing
      Right smallerCardsDrawn => Just $ Vect.index smallerCardsDrawn drawSequence
-}




stepDrawPhase : DrawPlayer -> DrawPlayer -> Maybe ClientInstruction
{-stepDrawPhase playerA playerB =
 map (\x => MkClientInstruction (serializeSequence x PlayerA, serializeSequence x PlayerB)) $ getCardDraw playerA playerB
-}


{-not sure where I handle whose turn it is...-}



notYourTurn : String
notYourTurn = "It is not your turn to draw a card!"

drawToSoulNotHand : String
drawToSoulNotHand = "You must a soul card this time. Select an unallocated slot among your five souls for your next card"


drawToHandNotSoul : String
drawToHandNotSoul = "You must draw a card for your hand this time. Make sure you have not selected any of your souls, and draw a card."

noSpellsInSoul : String
noSpellsInSoul = "None of your souls can be spell cards."


cardInvalid : String
cardInvalid = "The card you selected is not valid."

soulCardAlreadyDrawn : String
soulCardAlreadyDrawn = "You have already chosen a card for this soul. Select a card for a different soul."


invalidActionDrawPhase : String
invalidActionDrawPhase = "Invalid action during draw phase."

{-

A bit of the lore here: Mages have 5 souls, represented by their 5 soul cards.
When a mage has all 5 soul cards destroyed, they become "soulless".

This can change the nature of some abilities, usually making them more potent.

-}





{-Right now, cards only have their position in the card list, and no names, or ability to have images. This is a problem.
Also, for the purposes of reviving, we need to have multiple IDs for cards.

I guess name is being used (in the basic record)


odd that in the client update though, I have "cardId" rather than card name then...

-}

{-

assignSoulSkill : Fin 5 -> Nat -> String -> MonsterFactory -> DrawPlayer -> (DrawPlayer, List ClientUpdate)
{-assignSoulSkill soulIndex cardId playerId monsterFactory player =
 let player' = record {soulCards $= (\s => replaceAt soulIndex (Just (instantiateMonster cardId playerId monsterFactory)) s)} player in
 (player', [DrawSoul cardId soulIndex playerId])
-}

drawHandCard : Nat -> String -> CardFactory -> DrawPlayer -> (DrawPlayer, List ClientUpdate)
drawHandCard cardId playerId cardFactory player = ?hole
{-
 (record {hand $= (++[instantiateCardFactory cardId playerId cardFactory])} player, [DrawHand cardId playerId])
-}


{- for now, can also return nothing, until that is proven impossible. Nothing signifies an error in the game logic -}



{-
I also need to refactor all of my code to use Left for errors and Right for the nonfailing case

-}
-}

getActor : WhichPlayer -> DrawPlayer -> DrawPlayer -> DrawPlayer
getActor PlayerA a b = a
getActor PlayerB a b = b



transformDrawPhase :
 WhichPlayer ->
 DrawPlayer ->
 DrawPlayer ->
 ServerUpdate ->
 Maybe (Either (String, String) (DrawPlayer, List ClientUpdate))

-- getAllCardsDrawn commented out now temporarily or something. will fix later
{-

transformDrawPhase actor playerA playerB (DrawCardHand cardId) =
 let player = getActor actor playerA playerB in
 let playerId = temporaryId $ getActor actor playerA playerB in
 let cardsDrawn = List.length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB) in
 getCardDraw playerA playerB >>= \turn => {-this use of the monad, and also in the case below,
                                              seems incorrect, as getting Nothing here means an error, rather than no update needed.
                                              I guess nothing means internal error..
                                              -}
 if yourTurnToDraw turn actor
  then
  case fst turn of
   Soul => Just $ Left (drawToSoulNotHand, playerId)
   Hand =>
    case index' cardId cardList of
     Nothing => Just $ Left (cardInvalid, playerId)
     Just cardFactory => Just $ Right $ drawHandCard cardsDrawn playerId cardFactory player
 else
  Just $ Left (Draw_phase.notYourTurn, playerId)


transformDrawPhase actor playerA playerB (DrawCardSoul cardId soulIndex) =
 let player = getActor actor playerA playerB in
 let playerId = temporaryId player in
 let cardsDrawn = length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB) in
 getCardDraw playerA playerB >>= \turn =>
 if yourTurnToDraw turn actor then
  case fst turn of
   Hand =>
    Just $ Left (drawToHandNotSoul, playerId)
   Soul =>
    case index' cardId cardList of
     Nothing =>
      Just $ Left (cardInvalid, playerId)
     Just $ SpellCardFactory _ =>
      Just $ Left (noSpellsInSoul, playerId) 
     Just $ MonsterCardFactory monsterFactory => 
      case Vect.index soulIndex (soulCards player) of
       Just _ =>
        Just $ Left (soulCardAlreadyDrawn, playerId)
       Nothing =>
        Just $ Right $ assignSoulSkill soulIndex cardsDrawn playerId monsterFactory player
 else
  Just $ Left (Draw_phase.notYourTurn, playerId)

transformDrawPhase actor playerA playerB _  =
 let playerId = temporaryId $ getActor actor playerA playerB in
 getCardDraw playerA playerB >>= \turn =>
 if yourTurnToDraw turn actor
  then Just $ Left (invalidActionDrawPhase, playerId)
  else Just $ Left (Draw_phase.notYourTurn, playerId)


-}















