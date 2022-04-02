module MenuDecoder exposing (opMenuToDecode)

import Html exposing (Html, div, text)
import Json.Decode as Decode exposing (Decoder)
import Path exposing (Path)
import Route exposing (Route)
import View exposing (View)


decodeSegunTipoLiga seaExterna laDireccion =
    if seaExterna then
        Decode.succeed
            (laDireccion |> Path.fromString |> View.Otra)

    else
        case Route.urlToRoute { path = laDireccion } of
            Just cual ->
                Decode.succeed (View.Interna cual)

            Nothing ->
                Decode.fail <|
                    "Error no predefinida la ruta con la direccion interna: "
                        ++ laDireccion


decodificaDireccion : Bool -> Decoder View.LigaTipo
decodificaDireccion siEsExterna =
    Decode.field "dir" Decode.string
        |> Decode.andThen (decodeSegunTipoLiga siEsExterna)


decodificaLiga : Decoder View.LigaTipo
decodificaLiga =
    Decode.field "externa" Decode.bool
        |> Decode.andThen decodificaDireccion


ligasDecoder : Decoder (List View.Liga)
ligasDecoder =
    Decode.list
        (Decode.map3
            View.Liga
            decodificaLiga
            (Decode.field
                "queDice"
                Decode.string
            )
            (Decode.field
                "especial"
                Decode.bool
            )
        )


opMenuToDecode : { complementos | mainHero : Html (), afterHero : Html () } -> Decoder (View.MenuInfo ())
opMenuToDecode complementos =
    let
        decodeMenu : Decoder (View.MenuInfo ())
        decodeMenu =
            Decode.field
                "menu"
                (Decode.map2
                    View.SiMenu
                    ligasDecoder
                    (Decode.succeed
                        { mainHero = complementos.mainHero
                        , afterHero = complementos.afterHero
                        }
                    )
                )
    in
    Decode.field "menuGoes" Decode.bool
        |> Decode.andThen
            (\vaPues ->
                if vaPues then
                    decodeMenu

                else
                    Decode.succeed View.NoMenu
            )
