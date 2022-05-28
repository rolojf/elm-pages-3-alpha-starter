module Route.Contacto exposing (Data, Model, Msg, route)

import Analytics
import DataSource exposing (DataSource)
import Effect exposing (Effect)
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events as Events
import Http
import Json.Encode as Encode
import MiCloudinary
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (Path)
import Route
import RouteBuilder exposing (StatefulRoute, StatelessRoute, StaticPayload)
import Shared
import View exposing (View)


type alias RouteParams =
    {}


route : StatefulRoute RouteParams Data Model Msg
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithSharedState
            { view = view
            , update = superUpdate
            , subscriptions = subscriptions
            , init = init
            }


type alias Model =
    { nombre : String
    , comoSupo : String
    , correo : String
    , comentario : String
    , telefono : String
    , apellido : String
    , listo : Bool
    , respondio : Bool
    , deBasin : Result Http.Error String
    , intentos : Int
    , listoReto : Bool
    , intento : Intentos
    , queRespondio : String
    }


type Intentos
    = VaPues
    | RespondioMal
    | YaOk


type Msg
    = Nombre String
    | ComoSupo String
    | Correo String
    | Apellido String
    | Telefono String
    | Comentario String
    | CompletadoFormulario
    | EnfocaDespuesDeEsperar
    | NoOp
    | RespondeBasin (Result Http.Error String)
    | Notificado (Result Http.Error ())
    | IntentaDeNuez
    | Respondio String


init : Maybe PageUrl -> Shared.Model -> StaticPayload Data RouteParams -> ( Model, Effect Msg )
init maybePageUrl sharedModel static =
    ( { nombre = ""
      , comoSupo = ""
      , correo = ""
      , comentario = ""
      , telefono = ""
      , apellido = ""
      , listo = False
      , respondio = False
      , deBasin = Err (Http.BadStatus 9999)
      , intentos = 0
      , listoReto = False
      , intento = VaPues
      , queRespondio = ""
      }
    , Effect.none
    )


track : Msg -> Analytics.Event
track msg =
    case msg of
        CompletadoFormulario ->
            Analytics.eventoXReportar "completo-formulario"

        _ ->
            Analytics.none


superUpdate : PageUrl -> Shared.Model -> StaticPayload Data RouteParams -> Msg -> Model -> ( Model, Effect Msg, Maybe Shared.Msg )
superUpdate url sharedModel static msg model =
    let
        analyticsEvent : Analytics.Event
        analyticsEvent =
            track msg

        ( newModel, comandos, siSharedMsg ) =
            update url sharedModel static msg model
    in
    ( newModel
    , Effect.batch
        [ comandos
        , Analytics.toCmd
            analyticsEvent
            Notificado
        ]
    , siSharedMsg
    )



-- ( Model, Effect Msg, Maybe Shared.Msg )


update : PageUrl -> Shared.Model -> StaticPayload Data RouteParams -> Msg -> Model -> ( Model, Effect Msg, Maybe Shared.Msg )
update pageUrl sharedModel static msg model =
    let
        cuerpoPost : Encode.Value
        cuerpoPost =
            Encode.object
                [ ( "name", Encode.string model.nombre )
                , ( "apellido", Encode.string model.apellido )
                , ( "correo", Encode.string model.correo )
                , ( "telefono", Encode.string model.telefono )
                , ( "llego", Encode.string model.comoSupo )
                , ( "comentario", Encode.string model.comentario )
                ]
    in
    case msg of
        Nombre cCampo ->
            ( { model | nombre = cCampo }, Effect.none, Nothing )

        ComoSupo cCampo ->
            ( { model | comoSupo = cCampo }, Effect.none, Nothing )

        Correo cCampo ->
            ( { model | correo = cCampo }, Effect.none, Nothing )

        Apellido cCampo ->
            ( { model | apellido = cCampo }, Effect.none, Nothing )

        Telefono conQue ->
            let
                entered =
                    String.right 1 conQue

                conQue1 =
                    if String.contains entered "01234567890 _-.+" then
                        entered

                    else
                        ""
            in
            ( { model | telefono = String.dropRight 1 conQue ++ conQue1 }
            , Effect.none
            , Nothing
            )

        Comentario cCampo ->
            ( { model | comentario = cCampo }, Effect.none, Nothing )

        CompletadoFormulario ->
            ( { model | listo = True }
            , Effect.EsperaPues 150 EnfocaDespuesDeEsperar
            , Nothing
            )

        EnfocaDespuesDeEsperar ->
            ( model
            , Effect.Enfoca NoOp "valor-challenge"
            , Nothing
            )

        Respondio conQue ->
            let
                seLaSupo =
                    if conQue == "4" then
                        True

                    else
                        False
            in
            ( { model
                | queRespondio = conQue
                , intento =
                    if seLaSupo then
                        YaOk

                    else
                        RespondioMal
              }
              {- Http.post
                 { url = "https://usebasin.com/f/41489cfac434"
                 , body = Http.jsonBody cuerpoPost
                 , expect = Http.expectString RespondeBasin
              -}
            , Effect.EsperaPues
                500.0
                IntentaDeNuez
            , if seLaSupo then
                Ok "Todos Felices y Contentos"
                    |> Shared.Conocido
                    |> Shared.CambiaStatus
                    |> Shared.SharedMsg
                    |> Just

              else
                Nothing
            )

        IntentaDeNuez ->
            ( { model
                | queRespondio = ""
                , intentos = model.intentos + 1
              }
            , if model.intentos >= 3 || model.intento == YaOk then
                Route.toPath Route.Index
                    |> Pages.Url.fromPath
                    |> Pages.Url.toString
                    |> Effect.PushUrl NoOp

              else
                Effect.none
            , if model.intentos >= 3 then
                Just (Shared.SharedMsg <| Shared.CambiaStatus Shared.Rechazado)

              else
                Nothing
            )

        NoOp ->
            ( model
            , Effect.none
            , Nothing
            )

        RespondeBasin respuesta ->
            ( { model | deBasin = respuesta }
            , Route.toPath Route.Index
                |> Pages.Url.fromPath
                |> Pages.Url.toString
                |> Effect.PushUrl NoOp
            , Shared.Conocido respuesta
                |> Shared.CambiaStatus
                |> Shared.SharedMsg
                |> Just
            )

        Notificado resulto ->
            ( model
            , Effect.none
            , case resulto of
                Err quePaso ->
                    Just (Shared.SharedMsg <| Shared.ErrorAlNotificar quePaso)

                Ok _ ->
                    Nothing
            )


subscriptions : Maybe PageUrl -> RouteParams -> Path -> Shared.Model -> Model -> Sub Msg
subscriptions maybePageUrl routeParams path sharedModel model =
    Sub.none


type alias Data =
    {}


data : DataSource Data
data =
    DataSource.succeed Data


head : StaticPayload Data RouteParams -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view : Maybe PageUrl -> Shared.Model -> Model -> StaticPayload Data RouteParams -> View Msg
view maybeUrl sharedModel model static =
    { title = "Formulario de Contacto"
    , withMenu = View.NoMenu
    , body =
        [ div
            [ class "tw relative bg-white" ]
            [ viewLayout
            , viewFormulario model
            , if model.listo then
                div
                    [ class "tw lg:h-72" ]
                    [ if sharedModel.usuarioStatus == Shared.Desconocido then
                        viewChallenge model.intentos model.queRespondio model.intento

                      else
                        text ""
                    ]

              else
                div [] []
            ]
        ]
    }


viewLayout : Html Msg
viewLayout =
    div
        [ class "tw lg:absolute lg:inset-0" ]
        [ div
            [ class "tw lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2" ]
            [ Html.img
                [ class "tw h-56 w-full object-cover object-top lg:absolute lg:h-screen"
                , Attr.src <|
                    MiCloudinary.url
                        "f_auto"
                        "v1619940728/dreamstime_m_29668275_t0oapr.jpg"
                , Attr.alt ""
                ]
                []
            ]
        ]


viewFormulario : Model -> Html Msg
viewFormulario model =
    let
        viewCampoNombre =
            div
                []
                [ Html.label
                    [ Attr.for "first_name"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "Nombre" ]
                , div
                    [ class "mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "first_name"
                        , Attr.id "first_name"
                        , Attr.required True
                        , Attr.minlength 2
                        , Attr.maxlength 15
                        , Attr.autocomplete True -- "given-name"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Nombre
                        ]
                        []
                    ]
                ]

        viewCampoApellido =
            div []
                [ Html.label
                    [ Attr.for "last_name"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "Apellido" ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "last_name"
                        , Attr.id "last_name"
                        , Attr.autocomplete True -- "family-name"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Apellido
                        ]
                        []
                    ]
                ]

        viewCampoCorreo =
            div
                [ class "sm:col-span-2" ]
                [ Html.label
                    [ Attr.for "email"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "Correo Electrónico" ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.id "email"
                        , Attr.name "email"
                        , Attr.type_ "email"
                        , Attr.autocomplete True --"email"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Correo
                        ]
                        []
                    ]
                ]

        viewCampoTelefono =
            div
                [ class "tw sm:col-span-2" ]
                [ div
                    [ class "tw flex justify-between" ]
                    [ Html.label
                        [ Attr.for "phone"
                        , class "tw block text-sm font-medium text-gray-700"
                        ]
                        [ text "Teléfono" ]
                    , Html.span
                        [ Attr.id "phone_description"
                        , class "tw text-sm text-gray-500"
                        ]
                        [ text "Opcional" ]
                    ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "phone"
                        , Attr.id "phone"
                        , Attr.minlength 8
                        , Attr.maxlength 15
                        , Attr.value model.telefono
                        , Attr.autocomplete True -- "tel"
                        , Aria.ariaDescribedby "phone_description"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Telefono
                        ]
                        []
                    ]
                ]

        viewCampoComment =
            div
                [ class "tw sm:col-span-2" ]
                [ div
                    [ class "tw flex justify-between" ]
                    [ Html.label
                        [ Attr.for "how_can_we_help"
                        , class "tw block text-sm font-medium text-gray-700"
                        ]
                        [ text "Comentario" ]
                    , Html.span
                        [ Attr.id "how_can_we_help_description"
                        , class "tw text-sm text-gray-500"
                        ]
                        [ text ">Max. 500 caracteres" ]
                    ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.textarea
                        [ Attr.id "how_can_we_help"
                        , Attr.name "how_can_we_help"
                        , Aria.ariaDescribedby "how_can_we_help_description"
                        , Attr.rows 4
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border_gray_300 rounded-md"
                        , Events.onInput Comentario
                        ]
                        []
                    ]
                ]

        viewComoSupoDeNos =
            div
                [ class "tw sm:col-span-2" ]
                [ Html.label
                    [ Attr.for "how_did_you_hear_about_us"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "¿Cómo llegó con nosotros?" ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "how_did_you_hear_about_us"
                        , Attr.id "how_did_you_hear_about_us"
                        , class "tw shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w_full sm:text-sm border-gray-300 rounded-md"
                        , Events.onInput ComoSupo
                        ]
                        []
                    ]
                ]

        viewBotonSubmit =
            div
                [ class "tw text-right sm:col-span-2" ]
                [ Html.button
                    [ Attr.type_ "submit"
                    , class "tw inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    ]
                    [ text "Enviar" ]
                ]
    in
    div
        [ class "tw relative py-8 px-4 sm:px-6 lg:px-8 lg:max-w-7xl lg:mx-auto lg:grid lg:grid-cols-2 lg:py-8" ]
        [ div
            [ class "tw lg:pr-8" ]
            [ div
                [ class "tw max-w-md mx-auto lg:mx-0 sm:max-w-lg" ]
                [ Html.h2
                    [ class "tw text-3xl font-extrabold tracking-tight sm:text-4xl font-serif" ]
                    [ text "¿Cómo Podemos Ayudar?" ]
                , Html.p
                    [ class "tw mt-4 text-lg text-gray-500 sm:mt-3" ]
                    [ text "Responderemos tan pronto sea posible con un correo electrónico o con un mensaje a su teléfono. Gracias." ]
                , Html.form
                    [ Attr.action "#"
                    , Attr.method "POST"
                    , Events.onSubmit CompletadoFormulario
                    , class "tw mt-9 grid grid-cols-1 gap-y-6 sm:grid-cols-2 sm:gap-x-8"
                    ]
                    [ viewCampoNombre
                    , viewCampoApellido
                    , viewCampoCorreo
                    , viewCampoTelefono
                    , viewCampoComment
                    , viewComoSupoDeNos
                    , viewBotonSubmit
                    ]
                ]
            ]
        ]


viewChallenge : Int -> String -> Intentos -> Html Msg
viewChallenge cuantosIntentosVan respondioQue queHaRespondido =
    div
        [ class "la-base-modal" ]
        [ div
            [ class <|
                "tw bg-green-100 shadow rounded-lg mx-auto mt-24 w-10/12 h-64 md:max-w-md md:mx-auto md:mt-48"
                    ++ (if queHaRespondido == RespondioMal then
                            " tw animate-bounce"

                        else
                            ""
                       )
            ]
            [ Html.h3
                [ class "tw pt-4 ml-3 text-xl leading-6 font-medium text-gray-900 md:ml-6" ]
                [ text "Validación Rápida" ]
            , Html.p
                [ class "tw mt-2 mx-6 text-base leading-5 text-gray-500" ]
                [ Html.text "Contesta lo siguiente para validar que eres humano y no un bot" ]
            , div
                [ class "tw w-4/5 bg-yellow-100 mt-6 mx-auto h-32" ]
                [ Html.p
                    [ class "tw pt-5 pl-12 text-base font-medium text-gray-700" ]
                    [ Html.text "Resuleve la siguiente ecuación: " ]
                , div
                    [ class "tw ml-6 mt-4 flex flex-row items-center content-center justify-center text-base" ]
                    [ Html.p
                        []
                        [ Html.text "7 + " ]
                    , Html.label
                        [ class "tw sr-only"
                        , Attr.for "valor"
                        ]
                        [ Html.text "número" ]
                    , Html.input
                        [ class "tw text-center mx-2 w-5 rounded-md shadow-sm sm:leading-5 sm:text-sm"

                        -- Tw.block, Tw.w_full del .apparel-campo
                        , Attr.id "valor-challenge"
                        , Attr.autofocus True
                        , case queHaRespondido of
                            VaPues ->
                                Attr.placeholder "?"

                            RespondioMal ->
                                Attr.value respondioQue

                            YaOk ->
                                class "tw animate-ping"
                        , Events.onInput Respondio
                        ]
                        []
                    , Html.p
                        []
                        [ Html.text "= 11" ]
                    ]
                , if cuantosIntentosVan >= 1 then
                    Html.p
                        [ class <|
                            "text-right pt-4 mx-4 "
                                ++ (if cuantosIntentosVan == 1 then
                                        "tw text-black"

                                    else if cuantosIntentosVan == 2 then
                                        "tw text-red-500"

                                    else
                                        "tw text-red-500 font-bold italic"
                                   )
                        ]
                        [ text "Intenta de nuevo!" ]

                  else
                    Html.p [] []
                ]
            ]
        ]
