module Main.Draw_phase
import Data.Vect
import Main.Game
import Base.BoundedList
import Base.Player
import Base.Card
import Base.Clientupdates
import Main.Serverupdates

import Cards.Card_list

%access public export
%default total

data CardDraw = HA | HB | SA | SB

drawSequence : Vect 60 CardDraw
drawSequence = [
 HA, HB, HB, HA,   HB, HA, HA, HB,   HA, HB, SB, SA,
 HB, HA, HA, HB,   HA, HB, HB, HA,   HB, HA, SA, SB,
 HA, HB, HB, HA,   HB, HA, HA, HB,   HA, HB, SB, SA,
 HB, HA, HA, HB,   HA, HB, HB, HA,   HB, HA, SA, SB,
 HA, HB, HB, HA,   HB, HA, HA, HB,   HA, HB, SB, SA ]

yourHand : String
yourHand = "Select a card to add to your hand."
opponentHand : String
opponentHand = "Wait for your opponent to add a card to their hand."
yourSoul : String
yourSoul = "Select a position in your soul and a card to add to your soul."
opponentSoul : String
opponentSoul = "Wait for your opponent to add a card to their soul."

serializeSequence : CardDraw -> WhichPlayer -> String
serializeSequence HA PlayerA = yourHand
serializeSequence HA PlayerB = opponentHand
serializeSequence SA PlayerA = yourSoul
serializeSequence SA PlayerB = opponentSoul
serializeSequence HB PlayerA = opponentHand
serializeSequence HB PlayerB = yourHand
serializeSequence SB PlayerA = opponentSoul
serializeSequence SB PlayerB = yourSoul


{- move to player -}
getPlayer : WhichPlayer -> Player -> Player -> Player
getPlayer PlayerA playerA _ = playerA
getPlayer PlayerB _ playerB = playerB


getCardDraw : Player -> Player -> Maybe CardDraw
getCardDraw playerA playerB =
 let cardsDrawn = length (doIt (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) in
 case strengthen cardsDrawn of
      Left sameCardsDrawn => Nothing
      Right smallerCardsDrawn => Just $ Vect.index smallerCardsDrawn drawSequence

stepDrawPhase : Player -> Player -> Maybe ClientInstruction
stepDrawPhase playerA playerB =
 map (\x => MkClientInstruction (serializeSequence x PlayerA, serializeSequence x PlayerB)) $ getCardDraw playerA playerB

{-not sure where I handle whose turn it is...-}


transformDrawPhase : WhichPlayer -> Player -> Player -> ServerUpdate -> (Either (Game, List ClientUpdate) (String, String))
transformDrawPhase actor playerA playerB (DrawCardHand cardId) with (index' cardId cardList)
 | Nothing = Right ("The card you selected is not valid.", temporaryId $ getPlayer actor playerA playerB)
 | Just cardFactory = ?hole
transformDrawPhase actor playerA playerB (DrawCardSoul cardId soulIndex) with (index' cardId cardList)
 | Nothing = Right ("The card you selected is not valid.", temporaryId $ getPlayer actor playerA playerB)
 | Just (SpellCardFactory spellFactory) = ?hole  
 | Just (MonsterCardFactory monsterFactory) = ?hole
transformDrawPhase actor playerA playerB _  = Right ("Invalid action during draw phase.", temporaryId $ getPlayer actor playerA playerB)
