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
data ClientInstruction = MkClientInstruction (String,String) {- not integrated yet -}
-------------------------------------------------------------------------------

--hack for now
(==) : WhichPlayer -> WhichPlayer -> Bool
(==) PlayerA PlayerA = True
(==) PlayerB PlayerB = True
(==) _ _ = False

data ClientUpdate = GameLogicError
                 -- | RoundTerminated {-Currently don't progress to next round, and also don't distinguish players quite yet...-}
                  | GameTerminated WhichPlayer {-id of winning player-}
                  | MatchTerminated Result {-this update is only used by Ur/Web, in order to process ratings. I could remove this if Ur/Web kept track of round winners-}
                  | GameStart {-Not sure about arguments-}
                  | MatchStart WhichPlayer {-playerA, playerB-}
                  | DrawPhaseToSpawnPhase
                  | SpawnPhaseToSpellPhase
                  | SpellPhaseToRemovalPhase
                  | RemovalPhaseToStartPhase
                  | StartPhaseToEngagementPhase
                  | EngagementPhaseToEndPhase
                  | EndPhaseToRevivalPhase
                  | RevivalPhaseToDeploymentPhase
                  | DeploymentPhaseToSpawnPhase
                  | InvalidMove String String {-error message ; player id-}
                  {- | NotInAnyGame String -}
                  | Revive (Fin 9) WhichPlayer {-should have a list of cards to revive?-}
                  | Kill (Fin 9) WhichPlayer {-board index-}
                  | DeployCard (Fin 9) WhichPlayer
                  | DrawHand Nat WhichPlayer
                  | DrawSoul Nat (Fin 5) WhichPlayer
                  | SendSpawnToDiscard WhichPlayer
                  | MoveUnit (Fin 9) (Fin 9) WhichPlayer
                  | UpdateThoughts (Bounded 0 Preliminaries.absoluteUpperBound) WhichPlayer
                  | UpdateSchools (Vect 6 (Bounded 0 9)) WhichPlayer
                  | LoseSoulPoint WhichPlayer {- for now you only lose one at a time -}
                  | SendBoardToGraveyard (Fin 9) WhichPlayer
                  | SetStat String String (Fin 9) WhichPlayer {-stat name, marshalled stat value, board index -}
                  | SpawnCard Nat WhichPlayer
                  | PlayerTurn WhichPlayer

{-

data ClientUpdate = GameLogicError
                 -- | RoundTerminated {-Currently don't progress to next round, and also don't distinguish players quite yet...-}
                  | GameTerminated String {-id of winning player-}
                  | MatchTerminated Result {-this update is only used by Ur/Web, in order to process ratings. I could remove this if Ur/Web kept track of round winners-}
                  | GameStart {-Not sure about arguments-}
                  | MatchStart String String {-playerA, playerB-}
                  | DrawPhaseToSpawnPhase
                  | SpawnPhaseToSpellPhase
                  | SpellPhaseToRemovalPhase
                  | RemovalPhaseToStartPhase
                  | StartPhaseToEngagementPhase
                  | EngagementPhaseToEndPhase
                  | EndPhaseToRevivalPhase
                  | RevivalPhaseToDeploymentPhase
                  | DeploymentPhaseToSpawnPhase
                  | InvalidMove String String {-error message ; player id-}
                  {- | NotInAnyGame String -}
                  | Revive (Fin 9) String
                  | Kill (Fin 9) String {-board index, player id-}
                  | DeployCard (Fin 9) String
                  | DrawHand Nat String
                  | DrawSoul Nat (Fin 5) String
                  | SendSpawnToDiscard String
                  | MoveUnit (Fin 9) (Fin 9) String      
                  | UpdateThoughts (Bounded 0 Preliminaries.absoluteUpperBound) String
                  | UpdateSchools (Vect 6 (Bounded 0 9)) String
                  | LoseSoulPoint String {- for now you only lose one at a time -}
                  | SendBoardToGraveyard (Fin 9) String
                  | SetStat String String (Fin 9) String {-stat name, marshalled stat value, board index, player Id -}
                  | SpawnCard Nat String
                  | PlayerTurn String

-}


{-if it's expensive to write on pipes, some of this code could be moved into the Ur/Web-}
-------------------------------------------------------------------------------
getCardName : Nat -> Maybe String
getCardName permanentId = ?hole {- index the list of all cards -}
-------------------------------------------------------------------------------
record MarshalledClientUpdate where
 constructor MkMarshalledClientUpdate 
 type : String
 info : List (String, String) {-name of field, value of field.. for now no uniqueness guarantee at the type level-}
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

{-nothing if the user should not be receiving this update-}

marshallClientUpdate GameLogicError _ =
 MkMarshalledClientUpdate "gameLogicError" []
{-marshallClientUpdate RoundTerminated _ = Just $ MkMarshalledClientUpdate "roundTerminated" [] {-include data about the next round?-}-}

marshallClientUpdate DrawPhaseToSpawnPhase _ =
 MkMarshalledClientUpdate "drawPhaseToSpawnPhase" []

marshallClientUpdate SpawnPhaseToSpellPhase _ =
 MkMarshalledClientUpdate "spawnPhaseToSpellPhase" []

marshallClientUpdate SpellPhaseToRemovalPhase _ =
 MkMarshalledClientUpdate "spellPhaseToRemovalphase" []

marshallClientUpdate RemovalPhaseToStartPhase _ =
 MkMarshalledClientUpdate "removalPhaseToStartPhase" []

marshallClientUpdate StartPhaseToEngagementPhase _ =
 MkMarshalledClientUpdate "startPhaseToEngagementPhase" []

marshallClientUpdate EngagementPhaseToEndPhase _ =
 MkMarshalledClientUpdate "engagementPhaseToEndPhase" []

marshallClientUpdate EndPhaseToRevivalPhase _ =
 MkMarshalledClientUpdate "endPhaseToRevivalPhase" []

marshallClientUpdate RevivalPhaseToDeploymentPhase _ =
 MkMarshalledClientUpdate "revivalPhaseToDeploymentPhase" []

marshallClientUpdate DeploymentPhaseToSpawnPhase _ =
 MkMarshalledClientUpdate "deploymentPhaseToSpawnPhase" []

{-
{- removed while refactoring to not put playerIds in client messages -}
marshallClientUpdate (InvalidMove message playerId) id with (playerId == id)
  | False = Nothing
  | True = Just $ MkMarshalledClientUpdate "invalidMove" [("description",message)]
  -}


marshallClientUpdate (Revive boardIndex whichPlayer) player =
 let fields = [("index", show $ finToNat boardIndex)] in
 augment (MkMarshalledClientUpdate "revive" fields) (whichPlayer == player)

marshallClientUpdate (Kill boardIndex whichPlayer) player =
 let fields = [("index",show $ finToNat boardIndex)] in
 augment (MkMarshalledClientUpdate "kill" fields) (whichPlayer == player)

marshallClientUpdate (DeployCard boardIndex whichPlayer) player =
 let fields = [("index",show $ finToNat boardIndex)] in
 augment (MkMarshalledClientUpdate "deployCard" fields) (whichPlayer == player)
{-message currently 0 indexes the board-}

marshallClientUpdate (DrawHand cardId whichPlayer) player with (getCardName cardId)
 | Nothing = ?hole
 | Just cardName =
  let fields = [("name",cardName)] in
  augment (MkMarshalledClientUpdate "drawHandCard" fields) (whichPlayer == player)
  {-THIS is actually going to have a lot more data than the name: essentially all of the data of the card-}

marshallClientUpdate (DrawSoul cardId soulIndex whichPlayer) player with (getCardName cardId)
 | Nothing = ?hole
 | Just cardName = augment (MkMarshalledClientUpdate "drawSoulCard" [("name",cardName),("index",show $ finToNat soulIndex)]) (whichPlayer == player)

marshallClientUpdate (SendSpawnToDiscard whichPlayer) player =
 augment (MkMarshalledClientUpdate "sendSpawnToDiscard" []) (whichPlayer == player)

marshallClientUpdate (MoveUnit from to whichPlayer) player =
 let fields = [("from",show $ finToNat from),("to",show $ finToNat to)] in
 augment (MkMarshalledClientUpdate "moveUnit" fields) (whichPlayer == player)

marshallClientUpdate (UpdateThoughts val whichPlayer) player =
 let fields = [("val",show $ extractBounded val)] in
 augment (MkMarshalledClientUpdate "updateThoughts" fields) (whichPlayer == player)

marshallClientUpdate (UpdateSchools schools whichPlayer) player =
 let schoolNames = ["earth","fire","water","air","spirit","void"] in
 let generateField = (\x,y => (x,show $ extractBounded y)) in
 let fields = toList $ zipWith generateField schoolNames schools in
 augment (MkMarshalledClientUpdate "updateSchools" fields) (whichPlayer == player)

marshallClientUpdate (LoseSoulPoint whichPlayer) player =
 augment (MkMarshalledClientUpdate "loseSoulPoint" []) (whichPlayer == player)

marshallClientUpdate (SendBoardToGraveyard boardIndex whichPlayer) player =
 let boardIndexField = ("boardIndex",show $ finToInteger boardIndex) in
 let fields = [boardIndexField] in
 augment (MkMarshalledClientUpdate "sendBoardToGraveyard" fields) (whichPlayer == player)

marshallClientUpdate (SetStat stat val boardIndex whichPlayer) player =
 let statField = ("stat", stat) in
 let valField = ("val", val) in
 let indexField = ("index", show $ finToNat boardIndex) in
 let fields = [statField, valField, indexField] in
 augment (MkMarshalledClientUpdate "setStat" fields) (whichPlayer == player)

marshallClientUpdate (SpawnCard handIndex whichPlayer) player =
 let indexField = ("index", show handIndex) in
 let fields = [indexField] in
 augment (MkMarshalledClientUpdate "spawnCard" fields) (whichPlayer == player)

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



