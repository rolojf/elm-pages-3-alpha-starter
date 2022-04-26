module Reto exposing (..)

import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events


type alias Model =
    { intentos : Int
    , listo : Bool
    , intento : Intentos
    , queRespondio : String
    , vaDeNuez : Bool
    }


type Intentos
    = VaPues
    | YaRespondio
    | VaDeNuevo
    | EstaFrito
    | YaOk


newModel : Model
newModel =
    { intentos = 0
    , listo = False
    , intento = VaPues
    , queRespondio = ""
    , vaDeNuez = False
    }


type Msg
    = IntentaDeNuez
    | Respondio String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Respondio conQue ->
            let
                seLaSupo =
                    if conQue == "4" then
                        True

                    else
                        False
            in
            { model
                | queRespondio = conQue
                , intento =
                    if seLaSupo then
                        YaOk

                    else
                        YaRespondio
                , vaDeNuez = not seLaSupo
            }

        IntentaDeNuez ->
                { model
                    | queRespondio = ""
                    , intento = if model.intentos >= 3 then EstaFrito else VaDeNuevo
                    , vaDeNuez = False
                    , intentos = model.intentos + 1
                }



view : Model -> Html Msg
view model =
    div
        []
        [ viewChallenge model ]


viewChallenge : Model -> Html Msg
viewChallenge model =
    div
        [ class "la-base-modal" ]
        [ div
            [ class  <|
                "bg-green-100 shadow rounded-lg mx-auto mt-24 w-10/12 h-64 md:max-w-md md:mx-auto md:mt-48"
                    ++ (if model.intento == YaRespondio then
                            " animate-bounce"

                        else
                            ""
                       )
            ]
            [ Html.h3
                [ class "pt-4 ml-3 text-xl leading-6 font-medium text-gray-900 md:ml-6"  ]
                [ text "Validación Rápida" ]
            , Html.p
                [ class "mt-2 mx-6 text-base leading-5 text-gray-500"     ]
                [ Html.text "Contesta lo siguiente para validar que eres humano y no un bot" ]
            , div
                [ class "w-4/5 bg-yellow-100 mt-6 mx-auto h-32"                    ]
                [ Html.p
                    [ class "pt-5 pl-12 text-base font-medium text-gray-700"                    ]
                    [ Html.text "Resuleve la siguiente ecuación: " ]
                , div
                    [ class "ml-6 mt-4 flex flex-row items-center content-center justify-center text-base"                    ]
                    [ Html.p
                        []
                        [ Html.text "7 + " ]
                    , Html.label
                        [ class "sr-only"
                        , Attr.for "valor"
                        ]
                        [ Html.text "número" ]
                    , Html.input
                        [ class "text-center mx-2 w-5 rounded-md shadow-sm sm:leading-5 sm:text-sm"
                        -- Tw.block, Tw.w_full del .apparel-campo
                        , Attr.id "valor-challenge"
                        , Attr.autofocus True
                        , case model.intento of
                            VaPues ->
                                Attr.placeholder "?"

                            YaRespondio ->
                                Attr.value model.queRespondio

                            VaDeNuevo ->
                                Attr.value model.queRespondio

                            YaOk ->
                                class "animate-ping"

                            EstaFrito ->
                                class "no-se-que-decirle"
                        , Events.onInput Respondio
                        ]
                        []
                    , Html.p
                        []
                        [ Html.text "= 11" ]
                    ]
                , if model.intentos >= 1 then
                    Html.p
                        [ class <| "text-right pt-4 mx-4 "
                                ++ (if model.intentos == 1 then
                                        "text-black"

                                    else if model.intentos == 2 then
                                        "text-red-500"

                                    else
                                        "text-red-500 font-bold italic"
                                   )
                        ]
                        [ Html.text "Intenta de nuevo!" ]

                  else
                    Html.p [] []
                ]
            ]
        ]
