module Route.Index exposing (ActionData, Data, Model, Msg, route)

import Analytics
import DataSource exposing (DataSource)
import DataSource.File as File
import Effect exposing (Effect)
import Head
import Head.Seo as Seo
import HeroIcons
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block
import MdConverter
import MenuDecoder
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (Path)
import Route exposing (Route)
import RouteBuilder exposing (StatefulRoute, StaticPayload)
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import View exposing (View)



-- StatefulRoute routeParams data model msg


type alias Model =
    { verNotificaciones : StatusNotificacion }


type StatusNotificacion
    = NoStatusYet
    | ConStatusMostrar
    | ConStatusOcultar


type Msg
    = CierraNoti
    | NoOp
    | AvisadoAnalytics (Result Http.Error String)


type alias RouteParams =
    {}


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , update = update
            , subscriptions = subscriptions
            , init = init
            }


init : Maybe PageUrl -> Shared.Model -> StaticPayload Data RouteParams ActionData -> ( Model, Effect Msg )
init maybePageUrl sharedModel static =
    ( { verNotificaciones =
            if sharedModel.usuarioStatus == Shared.Desconocido then
                NoStatusYet

            else
                ConStatusMostrar
      }
    , Effect.none
    )


update : PageUrl -> Shared.Model -> StaticPayload Data ActionData RouteParams -> Msg -> Model -> ( Model, Effect Msg )
update pageUrl sharedModel static msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        CierraNoti ->
            ( { model | verNotificaciones = ConStatusOcultar }
            , Analytics.toEffect
                pageUrl
                (Analytics.eventoXReportar "cerro-notificacion")
                AvisadoAnalytics
            )

        AvisadoAnalytics resulto ->
            ( model
            , Effect.none
              {- , case resulto of
                 Err quePaso ->
                     Just (Shared.SharedMsg <| Shared.ErrorAlNotificar quePaso)

                 Ok _ ->
                     Nothing
              -}
            )


subscriptions : Maybe PageUrl -> RouteParams -> Path -> Shared.Model -> Model -> Sub Msg
subscriptions maybePageUrl routeParams path sharedModel model =
    Sub.none


type alias Data =
    { delMD : ContenidoConDatos }


type alias ContenidoConDatos =
    { body : Result String (List Markdown.Block.Block)
    , title : String
    , description : String
    , menu : View.MenuInfo (Pages.Msg.Msg Msg)
    }


data : DataSource Data
data =
    let
        miDecoder : String -> Decoder ContenidoConDatos
        miDecoder elCuerpo =
            Decode.map4 ContenidoConDatos
                (elCuerpo
                    |> MdConverter.parsea
                    |> Decode.succeed
                )
                (Decode.field "title" Decode.string)
                (Decode.field "description" Decode.string)
                (MenuDecoder.opMenuToDecode
                    { mainHero = div [] []
                    , afterHero = div [] []
                    }
                )

        getDataFromMD =
            File.bodyWithFrontmatter
                miDecoder
                "content/index.md"
    in
    DataSource.map Data
        getDataFromMD


head : StaticPayload Data ActionData RouteParams -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = static.sharedData.siteName
        , image =
            { url = [ "images", "icon-png.png" ] |> Path.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.delMD.description
        , locale = Shared.localito
        , title = static.data.delMD.title
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg Msg)
view maybeUrl sharedModel model static =
    { title =
        static.data.delMD.title
    , body =
        [ Html.h1 [] [ text "elm-pages is up and running!" ]
        , viewNotificacion sharedModel.usuarioStatus model.verNotificaciones
        , div
            [ class "tw prose" ]
            (MdConverter.renderea static.data.delMD.body)
            |> Html.map (\_ -> Pages.Msg.UserMsg NoOp)
        , Route.Blog__Slug_ { slug = "hola" }
            |> Route.link [] [ Html.text "My blog post" ]
        ]
    , withMenu =
        static.data.delMD.menu
    }



-- Notificaciones - modals


respFromPost : Result Http.Error String -> String
respFromPost resp =
    case resp of
        Ok _ ->
            "Registrado Ok, nos comunicaremos pronto."

        Err cual ->
            case cual of
                Http.BadUrl urlBad ->
                    "Pero, error en programa " ++ urlBad

                Http.Timeout ->
                    "No respondió el servidor, Intente de nuevo."

                Http.NetworkError ->
                    "Falló el internet."

                Http.BadStatus codigo ->
                    "Servidor regresó error " ++ String.fromInt codigo

                Http.BadBody infoEnviada ->
                    "Problemas con la información " ++ String.left 20 infoEnviada


viewNotificacion : Shared.UsuarioSt -> StatusNotificacion -> Html (Pages.Msg.Msg Msg)
viewNotificacion usrStatus verNotif =
    case usrStatus of
        Shared.Conocido respBasin ->
            retroFinal
                "Formulario Recibido"
                (respFromPost respBasin)
                verNotif

        Shared.Rechazado ->
            retroFinal
                "¡Información no registrada!"
                "Era necesario resolver la ecuación."
                verNotif

        Shared.Desconocido ->
            div [] []


notifAppear : StatusNotificacion -> Animation
notifAppear show =
    case show of
        NoStatusYet ->
            Animation.empty

        ConStatusMostrar ->
            Animation.fromTo
                { duration = 750
                , options =
                    [ Animation.delay 1100
                    , Animation.easeOut
                    ]
                }
                [ P.opacity 0, P.scale 0.92 ]
                [ P.opacity 1, P.scale 1 ]

        ConStatusOcultar ->
            Animation.fromTo
                { duration = 125
                , options = [ Animation.easeIn ]
                }
                [ P.opacity 1, P.scale 1, P.y 0.8 ]
                [ P.opacity 0, P.scale 0.92, P.y 0 ]


retroFinal : String -> String -> StatusNotificacion -> Html (Pages.Msg.Msg Msg)
retroFinal titulo subtitulo debeAparecer =
    Animated.div
        (notifAppear debeAparecer)
        [ Attr.attribute "aria-live" "assertive"
        , class "tw fixed inset-0 flex items-end px-4 py-6 z-20 pointer-events-none sm:p-6 lg:items-center"
        ]
        [ div
            [ class "tw w-full flex flex-col items-center space-y-4z sm:items-start lg:items-end" ]
            [ div
                [ class "tw max-w-sm w-full bg-gray-200 shadow-lg rounded-lg pointer-events-auto ring-1 ring-black ring-opacity-5 overflow-hidden" ]
                [ div
                    [ class "tw p-4" ]
                    [ div
                        [ class "tw flex items-start" ]
                        [ div
                            [ class "tw flex-shrink-0" ]
                            [ HeroIcons.outlineCheckCircle ]
                        , div
                            [ class "tw ml-3 w-0 flex-1 pt-0.5" ]
                            [ Html.p
                                [ class "tw text-sm font-medium text-gray-900" ]
                                [ text titulo ]
                            , Html.p
                                [ class "tw mt-1 text-sm text-gray-500" ]
                                [ text subtitulo ]
                            ]
                        , div
                            [ class "tw ml-4 flex-shrink-0 flex" ]
                            [ Html.button
                                [ class "tw bg-white rounded-md inline-flex text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                , Event.onClick (Pages.Msg.UserMsg CierraNoti)
                                ]
                                [ Html.span
                                    [ class "tw sr-only" ]
                                    [ text "Close" ]
                                , HeroIcons.solidX
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
