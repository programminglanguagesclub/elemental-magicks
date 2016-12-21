module Main.Draw_phase
import Data.Vect
import Main.Game
{-import Base.BoundedList-}
import Base.Player
import Base.Card
import Base.Clientupdates
import Main.Serverupdates

import Cards.Card_list
import Base.Phase
import Base.Skill_dsl_data

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


getCardDraw : Player -> Player -> Maybe CardDraw
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


noSpellsInSoul : String
noSpellsInSoul = "None of your souls can be spell cards."


cardInvalid : String
cardInvalid = "The card you selected is not valid."

{-

A bit of the lore here: Mages have 5 souls, represented by their 5 soul cards.
When a mage has all 5 soul cards destroyed, they become "soulless".

This can change the nature of some abilities, usually making them more potent.

-}



{-



 constructor MkGame
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead : Nonautomatic {-
 currentEvoker : Nat {-this would be better in skillHead but that is somewhat invasive when I create skills..-}
Actually I'm going to put this in them..
-}
 skillQueue : List Automatic
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 player_A : Player
 player_B : Player
 phase : Phase


-}


{- ClientUpdate

                 | DrawHand Nat String
                 | DrawSoul Nat (Fin 5) String

-}


{-Right now, cards only have their position in the card list, and no names, or ability to have images. This is a problem.
Also, for the purposes of reviving, we need to have multiple IDs for cards.

I guess name is being used (in the basic record)


odd that in the client update though, I have "cardId" rather than card name then...

-}




{-


instantiateMonster : Nat -> String -> MonsterFactory -> Monster
instantiateMonster cardId playerId monsterFactory =
  MkMonster (instantiateBasicMonster (basic monsterFactory) cardId)
            ((instantiateSkill cardId playerId) <$> (startSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (endSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (counterSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (spawnSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (deathSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (autoSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (actionSkills monsterFactory))
            (instantiateSkill cardId playerId (soulSkill monsterFactory))

instantiateSpell : Nat -> String -> SpellFactory -> Spell
instantiateSpell cardId playerId spellFactory =
  MkSpell (instantiateBasicSpell (basic spellFactory) cardId)
          (instantiateSkill cardId playerId (spawnSkill spellFactory))

data CardFactory = SpellCardFactory SpellFactory | MonsterCardFactory MonsterFactory
data Card = SpellCard Spell | MonsterCard Monster



-}



instantiateCardFactory : Nat -> String -> CardFactory -> Card
instantiateCardFactory cardId playerId (SpellCardFactory SpellFactory) = SpellCard $ instantiateSpell cardId playerId SpellFactory
instantiateCardFactory cardId playerId (MonsterCardFactory MonsterFactory) = MonsterCard $ instantiateMonster cardId playerId MonsterFactory


{- for now, can also return nothing, until that is proven impossible. Nothing signifies an error in the game logic -}
transformDrawPhase : WhichPlayer -> Player -> Player -> ServerUpdate -> Maybe (Either (Player, List ClientUpdate) (String, String))
transformDrawPhase actor playerA playerB (DrawCardHand cardId) with (index' cardId cardList)
 | Nothing = Just $ Right (cardInvalid, temporaryId $ getActor actor playerA playerB)
 | Just cardFactory = case getCardDraw playerA playerB of
                           Nothing => Nothing
                           Just HA => case actor of
                                      PlayerA => Just $ Left (record {hand $= (++[instantiateCardFactory (List.length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) (temporaryId $ getActor actor playerA playerB) cardFactory])} playerA, [DrawHand cardId (temporaryId $ getActor actor playerA playerB)])
                                      PlayerB => Just $ Right (notYourTurn, temporaryId $ getActor actor playerA playerB)
                           Just HB => case actor of
                                      PlayerA => Just $ Right (notYourTurn, temporaryId $ getActor actor playerA playerB)
                                      PlayerB => Just $ Left (record {hand $= (++[instantiateCardFactory (List.length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) (temporaryId $ getActor actor playerA playerB) cardFactory])} playerB, [DrawHand cardId (temporaryId $ getActor actor playerA playerB)])
                           Just SA => case actor of
                                      PlayerA => Just $ Right (drawToSoulNotHand, temporaryId $ getActor actor playerA playerB)
                                      PlayerB => Just $ Right (notYourTurn, temporaryId $ getActor actor playerA playerB)
                           Just SB => case actor of
                                      PlayerA => Just $ Right (notYourTurn, temporaryId $ getActor actor playerA playerB)
                                      PlayerB => Just $ Right (drawToSoulNotHand, temporaryId $ getActor actor playerA playerB) {- already know what player from matching on actor -}
transformDrawPhase actor playerA playerB (DrawCardSoul cardId soulIndex) with (index' cardId cardList)
 | Nothing = Just $ Right (cardInvalid, temporaryId $ getActor actor playerA playerB)
 | Just (SpellCardFactory spellFactory) = Just $ Right (noSpellsInSoul, temporaryId $ getActor actor playerA playerB)
 | Just (MonsterCardFactory monsterFactory) = case index soulIndex (soulCards $ getActor actor playerA playerB) of
                                                   Nothing => Just $ Left (record {soulCards $= (\s => replaceAt soulIndex (Just (instantiateMonster (List.length $ getAllCardsDrawn (soulCards playerA) (soulCards playerB) (hand playerA) (hand playerB)) (temporaryId $ getActor actor playerA playerB) monsterFactory)) s)} (getActor actor playerA playerB), [DrawSoul cardId soulIndex (temporaryId $ getActor actor playerA playerB)])
                                                   Just monsterCard => Just $ Right ("You have already chosen a card for this soul. Select a card for a different soul.", temporaryId $ getActor actor playerA playerB)


transformDrawPhase actor playerA playerB _  = Just $ Right ("Invalid action during draw phase.", temporaryId $ getActor actor playerA playerB)











