module Base.Clientupdates
import Data.Vect
import Base.Preliminaries
import Base.Bounded
%access public export
%default total

-------------------------------------------------------------------------------
data Result
 = Tie
 | OriginalPlayerAWon
 | OriginalPlayerBWon
-------------------------------------------------------------------------------
data ClientInstruction = MkClientInstruction (String,String,WhichPlayer)
                       -- not integrated yet
                       -- whichPlayer refers to whose turn it is now.
-------------------------------------------------------------------------------

--hack for now
-- REPLACE WITH DECEQ TYPECLASS
(==) : WhichPlayer -> WhichPlayer -> Bool
(==) PlayerA PlayerA = True
(==) PlayerB PlayerB = True
(==) _ _ = False

-------------------------------------------------------------------------------

-- might have already defined something called selection....
data Selection
 = Selected
 | Unselected

Show Selection where
 show Selected = "selected"
 show Unselected = "unselected"

data ClientUpdate
  = GameLogicError
                 -- | RoundTerminated {-Currently don't progress to next round, and also don't distinguish players quite yet...-}
  | GameTerminated WhichPlayer --winning player
  | MatchTerminated Result --this update is only used by Ur/Web, in order to process ratings. I could remove this if Ur/Web kept track of round winners
  | GameStart --Not sure about arguments
  | MatchStart WhichPlayer --playerA, playerB???
  | DrawPhaseToSpawnPhase
  | SpawnPhaseToSpellPhase
  | SpellPhaseToRemovalPhase
  | RemovalPhaseToStartPhase
  | StartPhaseToEngagementPhase
  | EngagementPhaseToEndPhase
  | EndPhaseToRevivalPhase
  | RevivalPhaseToDeploymentPhase
  | DeploymentPhaseToSpawnPhase
  | InvalidMove String String --error message ; player id
                  {- | NotInAnyGame String -}
  | Revive (Vect 9 Selection) WhichPlayer
  | Kill (Vect 9 Selection) WhichPlayer
  | DeployCard (Fin 9) WhichPlayer
  | DrawHand Nat WhichPlayer
  | DrawSoul Nat (Fin 5) WhichPlayer
  | SwapSpawnBoard (Fin 9) WhichPlayer
  | SwapSpawnHand (Fin 25) WhichPlayer
  | SwapSpawnGraveyard (Fin 25) WhichPlayer
  | SwapSpawnBanished (Fin 25) WhichPlayer
  | SwapBoardHand (Vect 9 (Either () (Fin 25))) WhichPlayer -- () if not selected. Fin 25 if selected to indicate where it goes.
  | SwapBoardGraveyard (Vect 9 (Either () (Fin 25))) WhichPlayer
  | SwapBoardBanished (Vect 9 (Either () (Fin 25))) WhichPlayer
  | SwapHandGraveyard (Fin 25) (Fin 25) WhichPlayer
  | SwapHandBanished (Fin 25) (Fin 25) WhichPlayer
  | SwapGraveyardBanished (Fin 25) (Fin 25) WhichPlayer
  | MoveUnit (Fin 9) (Fin 9) WhichPlayer
  | UpdateThoughts (Bounded 0 Preliminaries.absoluteUpperBound) WhichPlayer
  | UpdateSchools (Vect 6 (Bounded 0 9)) WhichPlayer
  | LoseSoulPoint WhichPlayer --for now you only lose one at a time
  | SetStat (Vect 9 (Maybe (String, String))) WhichPlayer
     --stat name, marshalled stat value
  | SpawnCard Nat WhichPlayer
  | UnspawnCard WhichPlayer
  | PlayerTurn WhichPlayer

--if it's expensive to write on pipes, some of this code could be moved into the Ur/Web
-------------------------------------------------------------------------------
getCardName : Nat -> Maybe String
getCardName permanentId = ?hole {- index the list of all cards -}
-------------------------------------------------------------------------------
record MarshalledClientUpdate where
 constructor MkMarshalledClientUpdate 
 type : String
 info : List (String, String)
  --name of field, value of field..
  --for now no uniqueness guarantee at the type level
-------------------------------------------------------------------------------
augment :
 MarshalledClientUpdate ->
 Bool ->
 MarshalledClientUpdate

augment marshalledClientUpdate b =
 let addPlayerInfo = (("player",if b then "player" else "opponent") ::) in
 record {info $= addPlayerInfo} marshalledClientUpdate
-------------------------------------------------------------------------------
marshallClientUpdate :
 ClientUpdate ->
 WhichPlayer ->
 MarshalledClientUpdate
                                                                             --
{-nothing if the user should not be receiving this update-}

marshallClientUpdate GameLogicError _ =
 MkMarshalledClientUpdate "gameLogicError" []
{-marshallClientUpdate RoundTerminated _ = Just $ MkMarshalledClientUpdate "roundTerminated" [] {-include data about the next round?-}-}
                                                                             --
marshallClientUpdate DrawPhaseToSpawnPhase _ =
 MkMarshalledClientUpdate "drawPhaseToSpawnPhase" []
                                                                             --
marshallClientUpdate SpawnPhaseToSpellPhase _ =
 MkMarshalledClientUpdate "spawnPhaseToSpellPhase" []
                                                                             --
marshallClientUpdate SpellPhaseToRemovalPhase _ =
 MkMarshalledClientUpdate "spellPhaseToRemovalphase" []
                                                                             --
marshallClientUpdate RemovalPhaseToStartPhase _ =
 MkMarshalledClientUpdate "removalPhaseToStartPhase" []
                                                                             --
marshallClientUpdate StartPhaseToEngagementPhase _ =
 MkMarshalledClientUpdate "startPhaseToEngagementPhase" []
                                                                             --
marshallClientUpdate EngagementPhaseToEndPhase _ =
 MkMarshalledClientUpdate "engagementPhaseToEndPhase" []
                                                                             --
marshallClientUpdate EndPhaseToRevivalPhase _ =
 MkMarshalledClientUpdate "endPhaseToRevivalPhase" []
                                                                             --
marshallClientUpdate RevivalPhaseToDeploymentPhase _ =
 MkMarshalledClientUpdate "revivalPhaseToDeploymentPhase" []
                                                                             --
marshallClientUpdate DeploymentPhaseToSpawnPhase _ =
 MkMarshalledClientUpdate "deploymentPhaseToSpawnPhase" []
                                                                             --
marshallClientUpdate (Revive selection whichPlayer) player =
 let fields = [("selections", show selection)] in
 augment (MkMarshalledClientUpdate "revive" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (Kill selection whichPlayer) player =
 let fields = [("index",show selection)] in
 augment (MkMarshalledClientUpdate "kill" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (DeployCard boardIndex whichPlayer) player =
 let fields = [("index",show $ finToNat boardIndex)] in
 augment (MkMarshalledClientUpdate "deployCard" fields) (whichPlayer == player)
{-message currently 0 indexes the board-}
                                                                             --
marshallClientUpdate (DrawHand cardId whichPlayer) player with (getCardName cardId)
 | Nothing = ?hole
 | Just cardName =
  let fields = [("name",cardName)] in
  augment (MkMarshalledClientUpdate "drawHandCard" fields) (whichPlayer == player)
  {-THIS is actually going to have a lot more data than the name: essentially all of the data of the card-}
                                                                             --
marshallClientUpdate (DrawSoul cardId soulIndex whichPlayer) player with (getCardName cardId)
 | Nothing = ?hole
 | Just cardName = augment (MkMarshalledClientUpdate "drawSoulCard" [("name",cardName),("index",show $ finToNat soulIndex)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapSpawnBoard boardIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapSpawnBoard" [("index", show boardIndex)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapSpawnHand handIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapSpawnHand" [("index", show handIndex)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapSpawnGraveyard graveyardIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapSpawnGraveyard" [("index", show graveyardIndex)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapSpawnBanished banishedIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapSpawnBanished" [("index", show banishedIndex)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapBoardHand indices whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapBoardHand" [("indices", show indices)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapBoardGraveyard indices whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapBoardGraveyard" [("indices", show indices)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapBoardBanished indices whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapBoardBanished" [("indices", show indices)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapHandGraveyard handIndex graveyardIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapHandGraveyard" [("handIndex", show handIndex), ("graveyardIndex", show graveyardIndex)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapHandBanished handIndex banishedIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapHandBanished" [("handIndex", show handIndex), ("banishedIndex", show banishedIndex)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SwapGraveyardBanished graveyardIndex banishedIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "swapGraveyardBanished" [("graveyardIndex", show graveyardIndex), ("banishedIndex", show banishedIndex)]) (whichPlayer == player)
                                                                             --


marshallClientUpdate (MoveUnit from to whichPlayer) player =
 let fields = [("from",show $ finToNat from),("to",show $ finToNat to)] in
 augment (MkMarshalledClientUpdate "moveUnit" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (UpdateThoughts val whichPlayer) player =
 let fields = [("val",show $ extractBounded val)] in
 augment (MkMarshalledClientUpdate "updateThoughts" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (UpdateSchools schools whichPlayer) player =
 let schoolNames = ["earth","fire","water","air","spirit","void"] in
 let generateField = (\x,y => (x,show $ extractBounded y)) in
 let fields = toList $ zipWith generateField schoolNames schools in
 augment (MkMarshalledClientUpdate "updateSchools" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (LoseSoulPoint whichPlayer) player =
 augment (MkMarshalledClientUpdate "loseSoulPoint" []) (whichPlayer == player)
                                                                             --
                                                                             --

-- | SetStat (Vect 9 (Maybe (String, String))) WhichPlayer

marshallClientUpdate (SetStat indices whichPlayer) player =
-- let statField = ("stat", stat) in
-- let valField = ("val", val) in
-- let indexField = ("index", show $ finToNat boardIndex) in
-- let fields = [statField, valField, indexField] in
 augment (MkMarshalledClientUpdate "setStat" [("indices", show indices)]) (whichPlayer == player)
                                                                             --
marshallClientUpdate (SpawnCard handIndex whichPlayer) player =
 let indexField = ("index", show handIndex) in
 let fields = [indexField] in
 augment (MkMarshalledClientUpdate "spawnCard" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (PlayerTurn whichPlayer) player =
 augment (MkMarshalledClientUpdate "playerTurn" []) (whichPlayer == player)
-------------------------------------------------------------------------------
serializeInfo : List (String,String) -> String
serializeInfo [] = ""
serializeInfo ((k,v)::xs) = "," ++ k ++ ":" ++ v ++ (serializeInfo xs)
-------------------------------------------------------------------------------
serializeMarshalled : MarshalledClientUpdate -> String

serializeMarshalled marshalledClientUpdate =
 let header = "updateType:" ++ (type marshalledClientUpdate) in
 let fields = serializeInfo (info marshalledClientUpdate) in
 "{" ++ header ++ fields ++ "}"
 
 {-player token added by ur/web-}
-------------------------------------------------------------------------------
serialize :
 ClientUpdate ->
 String ->
 String

serialize clientUpdate playerId =
 let marshalledClientUpdate = marshallClientUpdate clientUpdate ?hole in
 serializeMarshalled marshalledClientUpdate
-------------------------------------------------------------------------------
payload :
 ClientUpdate ->
 String ->
 String ->
 String

payload clientUpdate playerId opponentId =
 let playerMessage = serialize clientUpdate playerId in
 let opponentMessage = serialize clientUpdate opponentId in
 playerMessage ++ "~" ++ opponentMessage
-------------------------------------------------------------------------------



