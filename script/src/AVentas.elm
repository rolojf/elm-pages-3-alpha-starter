module AVentas exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Date exposing (Date)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Pages.Script as Script exposing (Script)
import Serialize as S
import Gen.Basics exposing (radians)
import Date exposing (toRataDie)
import Aventas exposing (..)


run : Script
run =
    File.rawFile "Fac3.json"
        |> BackendTask.allowFatal
        |> BackendTask.map parsea
        |> verPars
        |> BackendTask.andThen Script.log
        |> Script.withoutCliOptions


verPars =
    BackendTask.map
        (\x ->
            case x of
                Ok y ->
                    Debug.toString y

                Err z ->
                    Debug.toString z
        )

