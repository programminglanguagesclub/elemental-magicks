module Main.Draw_phase
import Data.Vect
import Main.Game
import Base.BoundedList
import Base.Player
import Base.Card
import Base.Clientupdates
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

{- deal with bounds checking later. For now assume okay -}



{- Currently there's an off by one error resulting from there not being a proper instruction if everything is fully draw -}

getCardDraw : Player -> Player -> Maybe CardDraw
getCardDraw playerA playerB =
 let cardsDrawn = length (doIt (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) in
 case strengthen cardsDrawn of
      Left sameCardsDrawn => Nothing
      Right smallerCardsDrawn => Just $ Vect.index smallerCardsDrawn drawSequence

{-
getCardDraw : Player -> Player -> CardDraw
getCardDraw playerA playerB =
 let handLengthA = length $ hand playerA in
 let handLengthB = length $ hand playerB in
 let soulLengthA = vectCount (soulCards playerA) isJust in
 let soulLengthB = vectCount (soulCards playerB) isJust in
 let hands = finSum handLengthA handLengthB in
 let souls = finSum soulLengthA soulLengthB in
 let everything = finSum hands souls in
 Vect.index everything drawSequence
  -}         

{-index {-((length $ hand playerA) + (length $ hand playerB))-} FZ drawSequence-}

{-
getMessageDrawPhase : Player -> Player -> Maybe (String,String)
getMessageDrawPhase playerA playerB = do 
                                      cardDraw <- getCardDraw playerA playerB
                                      pure (serializeSequence cardDraw PlayerA, serializeSequence cardDraw PlayerB)

-}

stepDrawPhase : Player -> Player -> Maybe ClientInstruction
stepDrawPhase playerA playerB = ?hole {-HAVE TO BE ABLE TO GO TO THE NEXT PHASE-}
