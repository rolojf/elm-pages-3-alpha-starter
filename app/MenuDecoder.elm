module MenuDecoder exposing (opMenuToDecode, decodificaLigas)

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

decodificaLigas =
    Decode.field "menu" ligasDecoder


{- En el markdown usamos cuatro campos
   espacial es la liga resaltada o con color primario.
   liga externa (True)o liga interna (False)
   Direccion en minúsculas.  Externa va con https://...
   interna va solo el nombre y/o submódulo.

   menu:
     -
       queDice: Inicio
       dir: ""
       especial: false
       externa: false
     -
       queDice: Sobre Nosotros
       dir: about
       especial: false
       externa: false

   ----------

   En data tenemos que tener un menu : View.MenuInfo Msg que es a lo que compila el decoder.

   En la función data, junto con otras cosas que se decodifican, se pasa el decodificador ahí definido así:

                   (MenuDecoder.opMenuToDecode
                       { mainHero = div [] []
                       , afterHero = div [] []
                       }
                   )
   Obviamente el mainHero y afterHero van definido según el menu en la parte principal. Para el menú básico va así.

-}
decodeMenu : { mainHero : Html msg, afterHero : Html msg } ->  Decoder (View.MenuInfo msg)
decodeMenu complementos =
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

opMenuToDecode : { mainHero : Html msg, afterHero : Html msg } -> Decoder (View.MenuInfo msg)
opMenuToDecode complementos =
    Decode.field "menuGoes" Decode.bool
        |> Decode.andThen
            (\vaPues ->
                if vaPues then
                    decodeMenu complementos

                else
                    Decode.succeed View.NoMenu
            )
