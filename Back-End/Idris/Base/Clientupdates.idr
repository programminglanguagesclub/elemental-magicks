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
                       -- the strings are the messages for players a and b
                       -- not in terms of who made the request because
                       -- I also use this in step game which doesn't know

generateClientInstruction :
 WhichPlayer ->
 String ->
 String ->
 ClientInstruction
generateClientInstruction whichPlayerOnMove onMoveMessage notOnMoveMessage with (whichPlayerOnMove)
 | PlayerA = MkClientInstruction (onMoveMessage, notOnMoveMessage, PlayerA)
 | PlayerB = MkClientInstruction (notOnMoveMessage, onMoveMessage, PlayerB)



notYourTurn : String
notYourTurn = "It is not your turn to act!"

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
  | GameTerminated WhichPlayer --winning player
  | DrawPhaseToSpawnPhase
  | SpawnPhaseToSpellPhase
  | SpellPhaseToRemovalPhase
  | RemovalPhaseToStartPhase
  | StartPhaseToEngagementPhase
  | EngagementPhaseToEndPhase
  | EndPhaseToRevivalPhase
  | RevivalPhaseToDeploymentPhase
  | DeploymentPhaseToSpawnPhase
  | InvalidMove String WhichPlayer
  | Revive (Vect 9 Selection) WhichPlayer
  | Kill (Vect 9 Selection) WhichPlayer
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

marshallClientUpdate GameTerminated winningPlayer = ?hole

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
   
marshallClientUpdate (InvalidMove errorMessage whichPlayer) player =
  let fields = [("errorMessage", errorMessage)] in
  augment
   (MkMarshalledClientUpdate "invalidMove" fields)
   (whichPlayer == player)
                                                                             --
marshallClientUpdate (Revive selection whichPlayer) player =
 let fields = [("selections", show selection)] in
 augment (MkMarshalledClientUpdate "revive" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (Kill selection whichPlayer) player =
 let fields = [("index",show selection)] in
 augment (MkMarshalledClientUpdate "kill" fields) (whichPlayer == player)
                                                                             --
marshallClientUpdate (DrawHand cardId whichPlayer) player =
  let fields = [("id",show cardId)] in
  augment (MkMarshalledClientUpdate "drawHandCard" fields) (whichPlayer == player)
  -- Ur/Web or Client is responsible for knowing database of all cards

marshallClientUpdate (DrawSoul cardId soulIndex whichPlayer) player =
 augment (MkMarshalledClientUpdate "drawSoulCard" [("id",show cardId),("index",show $ finToNat soulIndex)]) (whichPlayer == player)
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
marshallClientUpdate (SetStat indices whichPlayer) player =
-- let statField = ("stat", stat) in
-- let valField = ("val", val) in
-- let indexField = ("index", show $ finToNat boardIndex) in
-- let fields = [statField, valField, indexField] in
 augment (MkMarshalledClientUpdate "setStat" [("indices", show indices)]) (whichPlayer == player)
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
 String -> -- SHOULD THIS BE WHICHPLAYER AND NOT STRING???
 String

-- ALSO: How am I keeping track of which updates get send to 1 player versus 2, etc?

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



