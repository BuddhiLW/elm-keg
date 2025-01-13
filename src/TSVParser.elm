module TSVParser exposing (nodesTask, parseNodesTsv, parseSpaceTimestamp)

import BackendTask
import BackendTask.File as File exposing (FileReadError(..))
import Date exposing (Date)
import Dict exposing (Dict)
import FatalError exposing (FatalError)


type alias MyError =
    { fatal : FatalError
    , recoverable : FileReadError String
    }


parseSpaceTimestamp : String -> Result String Date
parseSpaceTimestamp raw =
    let
        isoString =
            String.replace " " "T" raw
    in
    case Date.fromIsoString isoString of
        Ok dateVal ->
            Ok dateVal

        Err msg ->
            Err ("Unable to parse date/time: " ++ raw ++ " (Reason: " ++ msg ++ ")")


parseNodesTsv :
    String
    -> List (Result String { entry : Int, published : Date, title : String })
parseNodesTsv content =
    -- identical to your existing parseNodesTsv code
    []


nodesTask :
    BackendTask.BackendTask
        MyError
        (List { entry : Int, published : Date, title : String })
nodesTask =
    -- identical to your existing nodesTask code
    BackendTask.succeed []
