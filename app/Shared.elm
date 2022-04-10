module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import DataSource
import Effect exposing (Effect)
import HeroIcons
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
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
    | ToggleMenu


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    { showMenu : Bool
    , showMenuInicial : Bool
    }


init :
    Pages.Flags.Flags
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
init flags maybePagePath =
    ( { showMenu = False
      , showMenuInicial = False
      }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SharedMsg globalMsg ->
            ( model, Effect.none )

        ToggleMenu ->
            ( { model
                | showMenuInicial = True
                , showMenu = not model.showMenu
              }
            , Effect.none
            )


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
            [ case pageView.withMenu of
                View.NoMenu ->
                    div [] []

                View.SiMenu ligasRecibidas _ ->
                    viewMenu ligasRecibidas model.showMenu model.showMenuInicial toMsg
            , Html.main_
                [ class "max-w-7xl mx-auto px-4 sm:px-6" ]
                pageView.body
            ]
    , title = pageView.title
    }


viewMenu : List View.Liga -> Bool -> Bool -> (Msg -> msg) -> Html msg
viewMenu ligas menuOpen byeMenu toMsg =
    let
        ligasNormales =
            List.filter (\liga -> liga.especial == False) ligas

        ligasEspeciales =
            List.filter .especial ligas

        setLink : String -> Html msg -> View.Liga -> Html msg
        setLink clases queHtml liga =
            case liga.dir of
                View.Otra camino ->
                    Html.a
                        [ Attr.href <| Path.toRelative camino
                        , class clases
                        ]
                        [ queHtml ]

                View.Interna rutaLiga ->
                    Route.link
                        [ class clases ]
                        [ queHtml ]
                        rutaLiga

        ligaNormalDesk : Html msg
        ligaNormalDesk =
            Html.nav
                [ class "hidden md:flex space-x-10" ]
                (List.map
                    (\cadaLiga ->
                        setLink
                            "text-base font-medium text-gray-500 hover:text-gray-900"
                            (text cadaLiga.queDice)
                            cadaLiga
                    )
                    ligasNormales
                )

        ligaEspecialDesk : Html msg
        ligaEspecialDesk =
            div
                [ class "hidden md:flex items-center justify-end md:flex-1 lg:w-0" ]
                (List.map
                    (\cadaLiga ->
                        setLink
                            "ml-8 whitespace-nowrap inline-flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700"
                            (text cadaLiga.queDice)
                            cadaLiga
                    )
                    ligasEspeciales
                )

        ligaNormalMovil =
            List.map
                (\cadaLiga ->
                    setLink
                        "-m-3 p-3 flex items-center rounded-md hover:bg-gray-50"
                        (Html.span
                            [ class "ml-3 text-base font-medium text-gray-900" ]
                            [ text cadaLiga.queDice ]
                        )
                        cadaLiga
                )
                ligasNormales

        ligaEspecialMovil =
            div []
                (List.map
                    (\cadaLiga ->
                        setLink
                            "w-full flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700"
                            (text cadaLiga.queDice)
                            cadaLiga
                    )
                    ligasEspeciales
                )

        showMovilMenu : Animation
        showMovilMenu =
            if menuOpen then
                Animation.fromTo
                    { duration = 580
                    , options = [ Animation.easeOut ]
                    }
                    [ P.opacity 0, P.scale 0.92 ]
                    [ P.opacity 1, P.scale 1 ]

            else
                Animation.fromTo
                    { duration = 525
                    , options = [ Animation.easeIn ]
                    }
                    [ P.opacity 1, P.scale 1 ]
                    [ P.opacity 0, P.scale 0.92 ]
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
                        , Event.onClick ToggleMenu
                        ]
                        [ Html.span
                            [ class "sr-only" ]
                            [ text "Open menu" ]
                        , HeroIcons.outlineMenu
                        ]
                        |> Html.map toMsg
                    ]
                , ligaNormalDesk
                , ligaEspecialDesk
                ]
            ]
        , if byeMenu then
            Animated.div
                showMovilMenu
                [ class "absolute top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden" ]
                [ div
                    [ class " bg-slate-100 rounded-lg shadow-lg ring-1 ring-black ring-opacity-5 bg-white divide-y-2 divide-gray-50" ]
                    [ div
                        [ class "pt-5 pb-6 px-5" ]
                        [ div
                            [ class "flex items-center justify-between" ]
                            [ div []
                                [ Html.img
                                    [ class "h-8 w-auto"
                                    , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                                    , Attr.alt "Workflow"
                                    ]
                                    []
                                ]
                            , div
                                [ class "-mr-2" ]
                                [ Html.button
                                    [ Attr.type_ "button"
                                    , Event.onClick ToggleMenu
                                    , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-blue-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                    ]
                                    [ Html.span
                                        [ class "sr-only" ]
                                        [ text "Close menu" ]
                                    , HeroIcons.outlineX
                                    ]
                                    |> Html.map toMsg
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

          else
            div [] []
        ]
