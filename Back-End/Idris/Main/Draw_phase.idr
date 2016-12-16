module Main.Draw_phase
import Main.Game
import Base.Player
import Base.Clientupdates
%access public export
%default total

data CardDraw = HA | HB | SA | SB

drawSequence : List CardDraw
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

getCardDraw : Player -> Player -> CardDraw
getCardDraw playerA playerB = index' ((length $ hand playerA) + (length $ hand playerB)) drawSequence

{-
getMessageDrawPhase : Player -> Player -> Maybe (String,String)
getMessageDrawPhase playerA playerB = do 
                                      cardDraw <- getCardDraw playerA playerB
                                      pure (serializeSequence cardDraw PlayerA, serializeSequence cardDraw PlayerB)

-}

stepDrawPhase : Player -> Player -> Maybe ClientInstruction
stepDrawPhase playerA playerB = ?hole
