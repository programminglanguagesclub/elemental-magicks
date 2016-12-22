module Main.Draw_phase
import Data.Vect
import Main.Game
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


getCardDraw : Player -> Player -> Maybe (CardDraw, WhichPlayer)
getCardDraw playerA playerB = List.index' (List.length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) drawSequence
 
 {-let cardsDrawn = length (doIt (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) in
 case strengthen cardsDrawn of
      Left sameCardsDrawn => Nothing
      Right smallerCardsDrawn => Just $ Vect.index smallerCardsDrawn drawSequence
-}




stepDrawPhase : Player -> Player -> Maybe ClientInstruction
stepDrawPhase playerA playerB =
 map (\x => MkClientInstruction (serializeSequence x PlayerA, serializeSequence x PlayerB)) $ getCardDraw playerA playerB

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





instantiateCardFactory : Nat -> String -> CardFactory -> Card
instantiateCardFactory cardId playerId (SpellCardFactory SpellFactory) = SpellCard $ instantiateSpell cardId playerId SpellFactory
instantiateCardFactory cardId playerId (MonsterCardFactory MonsterFactory) = MonsterCard $ instantiateMonster cardId playerId MonsterFactory


{- for now, can also return nothing, until that is proven impossible. Nothing signifies an error in the game logic -}
transformDrawPhase : WhichPlayer -> Player -> Player -> ServerUpdate -> Maybe (Either (Player, List ClientUpdate) (String, String))
transformDrawPhase actor playerA playerB (DrawCardHand cardId) =
 let playerId = temporaryId $ getActor actor playerA playerB in
 let cardsDrawn = length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB) in
 case (index' cardId cardList, getCardDraw playerA playerB, actor) of
      (Nothing,_,_) => Just $ Right (cardInvalid, playerId)
      (Just cardFactory,Nothing,_) => Nothing
      (Just cardFactory,Just (Hand, PlayerA),PlayerA) => Just $ Left (record {hand $= (++[instantiateCardFactory cardsDrawn playerId cardFactory])} playerA, [DrawHand cardId playerId])
      (Just cardFactory,Just (Hand, PlayerA),PlayerB) => Just $ Right (notYourTurn, playerId)
      (Just cardFactory,Just (Hand, PlayerB),PlayerA) => Just $ Right (notYourTurn, playerId)
      (Just cardFactory,Just (Hand, PlayerB),PlayerB) => Just $ Left (record {hand $= (++[instantiateCardFactory cardsDrawn playerId cardFactory])} playerB, [DrawHand cardId playerId])
      (Just cardFactory,Just (Soul, PlayerA),PlayerA) => Just $ Right (drawToSoulNotHand, playerId)
      (Just cardFactory,Just (Soul, PlayerA),PlayerB) => Just $ Right (notYourTurn, playerId)
      (Just cardFactory,Just (Soul, PlayerB),PlayerA) => Just $ Right (notYourTurn, playerId)
      (Just cardFactory,Just (Soul, PlayerB),PlayerB) => Just $ Right (drawToSoulNotHand, playerId)



transformDrawPhase actor playerA playerB (DrawCardSoul cardId soulIndex) =
 let player = getActor actor playerA playerB in
 let playerId = temporaryId player in
 let cardsDrawn = length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB) in
 case (index' cardId cardList, index soulIndex (soulCards player), getCardDraw playerA playerB, actor) of
      (_,_,Nothing,_) => Nothing
      (_,_,Just (Hand, PlayerA),PlayerA) => Just $ Right (drawToHandNotSoul, playerId)
      (_,_,Just (Hand, PlayerB),PlayerA) => Just $ Right (notYourTurn, playerId)
      (Nothing,_,Just (Soul, PlayerA),PlayerA) => Just $ Right (cardInvalid, playerId)
      (Just (SpellCardFactory spellFactory),_,Just (Soul, PlayerA),PlayerA) => Just $ Right (noSpellsInSoul, playerId)
      (Just (MonsterCardFactory monsterFactory),Just _,Just (Soul, PlayerA),PlayerA) => Just $ Right (soulCardAlreadyDrawn, playerId)
      (Just (MonsterCardFactory monsterFactory),Nothing,Just (Soul, PlayerA),PlayerA) => let player' = record {soulCards $= (\s => replaceAt soulIndex (Just (instantiateMonster cardsDrawn playerId monsterFactory)) s)} player in Just $ Left (player', [DrawSoul cardId soulIndex playerId])
      (_,_,Just (Soul, PlayerB),PlayerA) => Just $ Right (notYourTurn, playerId)
      (_,_,Just (Hand, PlayerA),PlayerB) => Just $ Right (notYourTurn, playerId)
      (_,_,Just (Hand, PlayerB),PlayerB) => Just $ Right (drawToHandNotSoul, playerId)
      (_,_,Just (Soul, PlayerA),PlayerB) => Just $ Right (notYourTurn, playerId)
      (Nothing,_,Just (Soul, PlayerB),PlayerB) => Just $ Right (cardInvalid, playerId)
      (Just (SpellCardFactory spellFactory),_,Just (Soul, PlayerB),PlayerB) => Just $ Right (noSpellsInSoul, playerId)      
      (Just (MonsterCardFactory monsterFactory),Just _,Just (Soul, PlayerB),PlayerB) => Just $ Right (soulCardAlreadyDrawn, playerId)
      (Just (MonsterCardFactory monsterFactory),Nothing,Just (Soul, PlayerB),PlayerB) => let player' = record {soulCards $= (\s => replaceAt soulIndex (Just (instantiateMonster cardsDrawn playerId monsterFactory)) s)} player in Just $ Left (player', [DrawSoul cardId soulIndex playerId])
      _ => ?hole

transformDrawPhase actor playerA playerB _  =
 let playerId = temporaryId $ getActor actor playerA playerB in
 case (getCardDraw playerA playerB, actor) of
      (Nothing,_) => Nothing
      (Just (Hand, PlayerA),PlayerA) => Just $ Right (invalidActionDrawPhase, playerId)
      (Just (Soul, PlayerA),PlayerA) => Just $ Right (invalidActionDrawPhase, playerId)
      (Just (Hand, PlayerB),PlayerB) => Just $ Right (invalidActionDrawPhase, playerId)
      (Just (Soul, PlayerB),PlayerB) => Just $ Right (invalidActionDrawPhase, playerId)
      (Just _,_) => Just $ Right (notYourTurn, playerId)



















