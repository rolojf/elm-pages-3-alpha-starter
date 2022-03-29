module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import Browser.Navigation
import DataSource
import Effect exposing (Effect)
import HeroIcons
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Nothing
    }


type Msg
    = SharedMsg SharedMsg
    | MenuClicked


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    { showMenu : Bool
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init navigationKey flags maybePagePath =
    ( { showMenu = False }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SharedMsg globalMsg ->
            ( model, Effect.none )

        MenuClicked ->
            ( { model | showMenu = not model.showMenu }, Effect.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html msg, title : String }
view sharedData page model toMsg pageView =
    { body =
        Html.div []
            [ Html.nav []
                [ Html.button
                    [ Html.Events.onClick MenuClicked ]
                    [ text
                        (if model.showMenu then
                            "Close Menu"

                         else
                            "Open Menu"
                        )
                    ]
                , if model.showMenu then
                    Html.ul [ class "bg-gray-300" ]
                        [ Html.li [ class "text-amber-300" ] [ text "Menu item 1" ]
                        , Html.li [] [ text "Menu item 2" ]
                        ]

                  else
                    Html.text ""
                ]
                |> Html.map toMsg
            , Html.main_ [] pageView.body
            ]
    , title = pageView.title
    }


viewMenu : List View.Liga -> Html msg
viewMenu ligas =
    let
        ligasNormales =
            List.filter (\liga -> liga.especial == False) ligas

        ligasEspeciales =
            List.filter .especial ligas

        ligaNormalDesk : Html msg
        ligaNormalDesk =
            Html.nav
                [ class "hidden md:flex space-x-10"
                ]
                (List.map
                    (\liga ->
                        case liga.dir of
                            View.Otra camino ->
                                Html.a
                                    [ Attr.href <| Path.toRelative camino
                                    , class "text-base font-medium text-gray-500 hover:text-gray-900"
                                    ]
                                    [ text liga.queDice ]

                            View.Interna rutaLiga ->
                                Route.link
                                    rutaLiga
                                    [ class "text-base font-medium text-gray-500 hover:text-gray-900" ]
                                    [ text liga.queDice ]
                    )
                    ligasNormales
                )

        ligaEspecialDesk : Html msg
        ligaEspecialDesk =
            div
                [ class "hidden md:flex items-center justify-end md:flex-1 lg:w-0" ]
                (List.map
                    (\liga ->
                        case liga.dir of
                            View.Otra camino ->
                                Html.a
                                    [ Attr.href <| Path.toRelative camino
                                    , class "ml-8 whitespace-nowrap inline-flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700"
                                    ]
                                    [ text liga.queDice ]

                            View.Interna rutaLiga ->
                                Route.link
                                    rutaLiga
                                    [ class "ml-8 whitespace-nowrap inline-flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700" ]
                                    [ text liga.queDice ]
                    )
                    ligasEspeciales
                )

        ligaNormalMovil =
            List.map
                (\liga ->
                    case liga.dir of
                        View.Otra camino ->
                            Html.a
                                [ Attr.href <| Path.toRelative camino
                                , class "-m-3 p-3 flex items-center rounded-md hover:bg-gray-50"
                                ]
                                [ Html.span
                                    [ class "ml-3 text-base font-medium text-gray-900" ]
                                    [ text liga.queDice ]
                                ]

                        View.Interna rutaLiga ->
                            Route.link
                                rutaLiga
                                [ class "-m-3 p-3 flex items-center rounded-md hover:bg-gray-50" ]
                                [ Html.span
                                    [ class "ml-3 text-base font-medium text-gray-900" ]
                                    [ text liga.queDice ]
                                ]
                )
                ligasNormales

        ligaEspecialMovil =
            div []
                (List.map
                    (\liga ->
                        case liga.dir of
                            View.Otra camino ->
                                Html.a
                                    [ Attr.href <| Path.toRelative camino
                                    , class "w-full flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700"
                                    ]
                                    [ text liga.queDice ]

                            View.Interna rutaLiga ->
                                Route.link
                                    rutaLiga
                                    [ class "w-full flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700" ]
                                    [ text liga.queDice ]
                    )
                    ligasEspeciales
                )
    in
    div
        [ class "relative bg-white"
        ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6" ]
            [ div [ class "flex justify-between items-center border-b-2 border-gray-100 py-6 md:justify-start md:space-x-10" ]
                [ div [ class "flex justify-start lg:w-0 lg:flex-1" ]
                    [ Html.a
                        [ Attr.href "#" ]
                        [ Html.span
                            [ class "sr-only" ]
                            [ text "Workflow" ]
                        , Html.img
                            [ class "h-8 w-auto sm:h-10"
                            , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                            , Attr.alt ""
                            ]
                            []
                        ]
                    ]
                , div
                    [ class "-mr-2 -my-2 md:hidden" ]
                    [ Html.button
                        [ Attr.type_ "button"
                        , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                        , Attr.attribute "aria-expanded" "false"
                        ]
                        [ Html.span
                            [ class "sr-only" ]
                            [ text "Open menu" ]
                        , HeroIcons.outlineMenu
                        ]
                    ]
                , ligaNormalDesk
                , ligaEspecialDesk
                ]
            ]
        , {-
             Mobile menu, show/hide based on mobile menu state.

             Entering: "duration-200 ease-out"
               From: "opacity-0 scale-95"
               To: "opacity-100 scale-100"
             Leaving: "duration-100 ease-in"
               From: "opacity-100 scale-100"
               To: "opacity-0 scale-95"
          -}
          div
            [ class "absolute top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden"
            ]
            [ div
                [ class "rounded-lg shadow-lg ring-1 ring-black ring-opacity-5 bg-white divide-y-2 divide-gray-50"
                ]
                [ div
                    [ class "pt-5 pb-6 px-5"
                    ]
                    [ div
                        [ class "flex items-center justify-between"
                        ]
                        [ div []
                            [ Html.img
                                [ class "h-8 w-auto"
                                , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                                , Attr.alt "Workflow"
                                ]
                                []
                            ]
                        , div
                            [ class "-mr-2"
                            ]
                            [ Html.button
                                [ Attr.type_ "button"
                                , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-blue-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                ]
                                [ Html.span
                                    [ class "sr-only"
                                    ]
                                    [ text "Close menu" ]
                                , HeroIcons.outlineX
                                ]
                            ]
                        ]
                    , div
                        [ class "mt-6" ]
                        [ Html.nav
                            [ class "grid gap-y-8" ]
                            ligaNormalMovil
                        ]
                    ]
                , div
                    [ class "py-6 px-5 space-y-6"
                    ]
                    [ ligaEspecialMovil
                    ]
                ]
            ]
        ]
