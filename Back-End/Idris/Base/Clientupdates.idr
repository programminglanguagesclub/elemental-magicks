module Base.Clientupdates
import Data.Vect
import Base.Preliminaries
import Base.Bounded
%access public export
%default total

data Result
 = Tie
 | OriginalPlayerAWon
 | OriginalPlayerBWon

data ClientInstruction = MkClientInstruction (String,String) {- not integrated yet -}

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


{-if it's expensive to write on pipes, some of this code could be moved into the Ur/Web-}
getCardName : Nat -> Maybe String
getCardName permanentId = ?hole {- index the list of all cards -}

record MarshalledClientUpdate where
 constructor MkMarshalledClientUpdate 
 type : String
 info : List (String, String) {-name of field, value of field.. for now no uniqueness guarantee at the type level-}
augment : MarshalledClientUpdate -> Bool -> MarshalledClientUpdate
augment marshalledClientUpdate b = record {info = ("player",if b then "player" else "opponent") :: (info marshalledClientUpdate)} marshalledClientUpdate
-------------------------------------------------------------------------------
marshallClientUpdate :
 ClientUpdate ->
 String ->
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


marshallClientUpdate (Revive boardIndex playerId) id =
 augment (MkMarshalledClientUpdate "revive" [("index", show $ finToNat boardIndex)]) (playerId == id)
marshallClientUpdate (Kill boardIndex playerId) id =
 augment (MkMarshalledClientUpdate "kill" [("index",show $ finToNat boardIndex)]) (playerId == id)
marshallClientUpdate (DeployCard boardIndex playerId) id =
 augment (MkMarshalledClientUpdate "deployCard" [("index",show $ finToNat boardIndex)]) (playerId == id)
{-message currently 0 indexes the board-}


marshallClientUpdate (DrawHand cardId playerId) id with (getCardName cardId)
 | Nothing = ?hole
 | Just cardName = augment (MkMarshalledClientUpdate "drawHandCard" [("name",cardName)]) (playerId == id)
  {-THIS is actually going to have a lot more data than the name: essentially all of the data of the card-}
marshallClientUpdate (DrawSoul cardId soulIndex playerId) id with (getCardName cardId)
 | Nothing = ?hole
 | Just cardName = augment (MkMarshalledClientUpdate "drawSoulCard" [("name",cardName),("index",show $ finToNat soulIndex)]) (playerId == id)
marshallClientUpdate (SendSpawnToDiscard playerId) id =
 augment (MkMarshalledClientUpdate "sendSpawnToDiscard" []) (playerId == id)
marshallClientUpdate (MoveUnit from to playerId) id =
 augment (MkMarshalledClientUpdate "moveUnit" [("from",show $ finToNat from),("to",show $ finToNat to)]) (playerId == id)
marshallClientUpdate (UpdateThoughts val playerId) id =
 augment (MkMarshalledClientUpdate "updateThoughts" [("val",show $ extractBounded val)]) (playerId == id)
marshallClientUpdate (UpdateSchools schools playerId) id =
 augment (MkMarshalledClientUpdate "updateSchools" (toList $ zipWith (\x,y => (x,show $ extractBounded y)) ["earth","fire","water","air","spirit","void"] schools)) (playerId == id)
marshallClientUpdate (LoseSoulPoint playerId) id =
 augment (MkMarshalledClientUpdate "loseSoulPoint" []) (playerId == id)
marshallClientUpdate (SendBoardToGraveyard boardIndex playerId) id =
 augment (MkMarshalledClientUpdate "sendBoardToGraveyard" [("boardIndex",show $ finToInteger boardIndex)]) (playerId == id)
marshallClientUpdate (SetStat stat val boardIndex playerId) id =
 augment (MkMarshalledClientUpdate "setStat" [("stat",stat),("val",val),("index",show $ finToNat boardIndex)]) (playerId == id)
marshallClientUpdate (SpawnCard handIndex playerId) id =
 augment (MkMarshalledClientUpdate "spawnCard" [("index",show handIndex)]) (playerId == id)
marshallClientUpdate (PlayerTurn playerId) id =
 augment (MkMarshalledClientUpdate "playerTurn" []) (playerId == id)
-------------------------------------------------------------------------------
serializeInfo : List (String,String) -> String
serializeInfo [] = ""
serializeInfo ((k,v)::xs) = "," ++ k ++ ":" ++ v ++ (serializeInfo xs)
-------------------------------------------------------------------------------
serializeMarshalled : MarshalledClientUpdate -> String
serializeMarshalled marshalledClientUpdate = "{updateType:" ++ (type marshalledClientUpdate) ++ (serializeInfo (info marshalledClientUpdate)) ++ "}" {-player token added by ur/web-}
-------------------------------------------------------------------------------
serialize : ClientUpdate -> String -> String
serialize clientUpdate playerId =
 let marshalledClientUpdate = marshallClientUpdate clientUpdate playerId in
 serializeMarshalled marshalledClientUpdate
-------------------------------------------------------------------------------
payload : ClientUpdate -> String -> String -> String
payload clientUpdate playerId opponentId =
 let playerMessage = serialize clientUpdate playerId in
 let opponentMessage = serialize clientUpdate opponentId in
 playerMessage ++ "~" ++ opponentMessage
-------------------------------------------------------------------------------



