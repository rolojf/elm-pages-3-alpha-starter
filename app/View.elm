module View exposing
    ( View, map
    , Liga, LigaTipo(..), MenuComplemento, MenuInfo(..)
    )

{-|

@docs View, map, placeholder

-}

import Html exposing (Html)
import Path exposing (Path)
import Route exposing (Route)


{-| -}
type alias View msg =
    { title : String
    , body : List (Html msg)
    , withMenu : MenuInfo msg
    }


type MenuInfo msg
    = NoMenu
    | SiMenu (List Liga) (MenuComplemento msg)


type alias MenuComplemento msg =
    { mainHero : Html msg
    , afterHero : Html msg
    }


type alias Liga =
    { dir : LigaTipo
    , queDice : String
    , especial : Bool
    }


type LigaTipo
    = Otra Path
    | Interna Route


{-| -}
map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    , withMenu =
        case doc.withMenu of
            NoMenu ->
                NoMenu

            SiMenu ligas complementosAlMenu ->
                SiMenu
                    ligas
                    { mainHero = Html.map fn complementosAlMenu.mainHero
                    , afterHero = Html.map fn complementosAlMenu.afterHero
                    }
    }
