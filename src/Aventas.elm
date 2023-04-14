module Aventas exposing (Registro, parsea, registroCodec)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Date exposing (Date)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Pages.Script as Script exposing (Script)
import Serialize as S
import Gen.Basics exposing (radians)
import Date exposing (toRataDie)



{-run : script
run =
    file.rawfile "fac3.json"
        |> backendtask.allowfatal
        |> backendtask.map parsea
        |> verpars
        |> backendtask.andthen script.log
        |> script.withoutclioptions
-}


type alias Registro =
    { oti : Int
    , oti3 : Int
    , modelo : String
    , descripcion : String
    , partida : Int
    , cantidad : Int
    , factura : Int
    , fecha : Date
    , fechaEntrega : Date
    , fechaFactura :  Date
    }


-- using elm-serialize from MartinSStewart write codec for Registro record
registroCodec : S.Codec e Registro
registroCodec =
    let
        dateCodec =
            S.int
            |> S.map
               Date.fromRataDie
               Date.toRataDie

    in
    S.record Registro
        |> S.field .oti S.int
        |> S.field .oti3 S.int
        |> S.field .modelo S.string
        |> S.field .descripcion S.string
        |> S.field .partida S.int
        |> S.field .cantidad S.int
        |> S.field .factura S.int
        |> S.field .fecha dateCodec
        |> S.field .fechaEntrega dateCodec
        |> S.field .fechaFactura dateCodec
        |> S.finishRecord


fechaProc : String -> Date
fechaProc fff =
    fff
        |> String.split "/"
        |> List.reverse
        |> (\lista ->
                case lista of
                    [] ->
                        ""

                    x :: xs ->
                        x ++ "-" ++ (List.reverse >> String.join "-") xs
           )
        |> Date.fromIsoString
        |> Result.withDefault (Date.fromRataDie 1)


parsea : String -> Result D.Error (List Registro)
parsea =
    let
        parser =
            D.succeed Registro
                |> DP.required "OTI"
                    (D.maybe D.int
                        |> D.map (Maybe.withDefault 0)
                    )
                |> DP.required "OTI3"
                    (D.maybe D.int
                        |> D.map (Maybe.withDefault 0)
                    )
                |> DP.required "Modelo"
                    (D.maybe D.string
                        |> D.map (Maybe.withDefault "xyz" >> String.trim)
                    )
                |> DP.required "Descripcion"
                    (D.maybe D.string
                        |> D.map (Maybe.withDefault "xyz" >> String.trim)
                    )
                |> DP.required "Partida"
                    (D.maybe D.int
                        |> D.map (Maybe.withDefault 0)
                    )
                |> DP.required "Cantidad"
                    (D.maybe D.int
                        |> D.map (Maybe.withDefault 0)
                    )
                |> DP.required "Factura"
                    (D.maybe D.int
                        |> D.map (Maybe.withDefault 0)
                    )
                |> DP.required "Fecha"
                    (D.string
                        |> D.map (String.trim >> fechaProc)
                    )
                |> DP.required "FechaEntregaReal"
                    (D.string
                        |> D.map (String.trim >> fechaProc)

                    )
                |> DP.required "FechaFactura"
                    (D.maybe D.string
                        |> D.map (Maybe.withDefault "xyz" >> String.trim >> fechaProc)
                    )
    in
    D.decodeString
        (D.list
            parser
        )
