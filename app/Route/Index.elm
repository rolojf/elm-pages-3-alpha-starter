module Route.Index exposing (ActionData, Data, Model, Msg, route)

-- * Imports

import Analytics
import Array exposing (Array)
import Browser.Dom as Dom
import DataSource exposing (DataSource)
import DataSource.File as File
import Effect exposing (Effect)
import ErroresHttp
import Footer
import HardCodedData
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
import MiCloudinary
import MimeType exposing (MimeType)
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
import Svg
import Svg.Attributes as SvgAttr
import Task
import View exposing (View)



-- * StatefulRoute routeParams data model msg


type alias Model =
    { verNotificaciones : StatusNotificacion
    , menuOpen : Bool
    , showSlider : Bool
    , avanzoManual : Bool
    , dirAvance : DirAvanceManual
    , inicializado : Bool
    , cualSlideActivo : Int
    , aminar : Amimacion
    , cambia : Int
    , cuantasFotos : Int
    }


type Amimacion
    = Entra
    | Sale


type DirAvanceManual
    = None
    | Izq
    | Der


type StatusNotificacion
    = NoStatusYet
    | ConStatusMostrar
    | ConStatusOcultar


type Msg
    = CierraNoti
    | NoOp
    | AvisadoAnalytics (Result Http.Error String)
    | ToggleMenu
    | CheckGalInView (Result Dom.Error Dom.Element)
    | WaitToCheckAgainGalInView
    | WaitToGalAutoRotate
    | Avanza
    | Retrocede
    | Para
    | PresionoBotonIzq
    | PresionoBotonDer
    | Notificado (Result Http.Error String)
    | AnalyticsUsoMenuLigaExterna String


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
        |> RouteBuilder.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = subscriptions
            }


init : Maybe PageUrl -> Shared.Model -> StaticPayload Data ActionData RouteParams -> ( Model, Effect Msg )
init maybePageUrl sharedModel static =
    ( { menuOpen = False
      , verNotificaciones =
            if sharedModel.usuarioStatus == Shared.Desconocido then
                NoStatusYet

            else
                ConStatusMostrar
      , showSlider = False
      , avanzoManual = False
      , dirAvance = None
      , inicializado = False
      , cualSlideActivo = 0
      , aminar = Entra
      , cambia = 0
      , cuantasFotos = Array.length textosGal
      }
    , Effect.none
      -- Task.attempt CheckGalInView (Dom.getElement "slider-container")
    )



-- * Update


update : PageUrl -> Shared.Model -> StaticPayload Data ActionData RouteParams -> Msg -> Model -> ( Model, Effect Msg, Maybe Shared.Msg )
update pageUrl sharedModel static msg model =
    case msg of
        NoOp ->
            ( model, Effect.none, Nothing )

        CierraNoti ->
            ( { model | verNotificaciones = ConStatusOcultar }
            , Analytics.toEffect
                pageUrl
                (Analytics.eventoXReportar "cerro-notificacion")
                AvisadoAnalytics
            , Nothing
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
            , Nothing
            )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }
            , Effect.none
            , Nothing
            )

        CheckGalInView resultaPos ->
            let
                galInSight pos =
                    (pos.element.y - 0.7 * pos.viewport.height) < pos.viewport.y

                onView : Maybe Float
                onView =
                    case resultaPos of
                        Ok pos ->
                            if galInSight pos then
                                Nothing

                            else
                                Just (1.0 - (pos.viewport.y / pos.element.y))

                        _ ->
                            Just 1.0
            in
            ( { model
                | showSlider =
                    if onView == Nothing then
                        True

                    else
                        False
              }
            , case onView of
                Just waitingTime ->
                    Effect.EsperaPues
                        (200000 * waitingTime)
                        WaitToCheckAgainGalInView

                -- debe ser 2000
                Nothing ->
                    Effect.EsperaPues
                        6500
                        WaitToGalAutoRotate
            , Nothing
            )

        WaitToCheckAgainGalInView ->
            ( model
            , Task.attempt CheckGalInView (Dom.getElement "slider-container") |> Effect.fromCmd
            , Nothing
            )

        WaitToGalAutoRotate ->
            if model.avanzoManual then
                ( { model | avanzoManual = False }
                , Effect.EsperaPues 12500 WaitToGalAutoRotate
                , Nothing
                )

            else
                ( model
                , Effect.batch
                    [ Effect.EsperaPues 6500 WaitToGalAutoRotate
                    , Effect.Success
                        (if model.dirAvance == Izq then
                            Retrocede

                         else
                            Avanza
                        )
                    ]
                , Nothing
                )

        PresionoBotonIzq ->
            update
                pageUrl
                sharedModel
                static
                Retrocede
                { model
                    | dirAvance = Izq
                    , avanzoManual = True
                }

        PresionoBotonDer ->
            update
                pageUrl
                sharedModel
                static
                Avanza
                { model
                    | dirAvance = Der
                    , avanzoManual = True
                }

        Avanza ->
            ( { model
                | cambia =
                    if model.showSlider then
                        1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                Effect.EsperaPues 1300 Para

              else
                Effect.Success Para
            , Nothing
            )

        Retrocede ->
            ( { model
                | cambia =
                    if model.showSlider then
                        -1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                Effect.EsperaPues 1300 Para

              else
                Effect.Success Para
            , Nothing
            )

        Para ->
            let
                nuevoSlideActivo : Int
                nuevoSlideActivo =
                    model.cualSlideActivo + model.cambia

                nuevoSlideActivoValidado : Int
                nuevoSlideActivoValidado =
                    if nuevoSlideActivo < 0 then
                        model.cuantasFotos - 1

                    else if nuevoSlideActivo == model.cuantasFotos then
                        0

                    else
                        nuevoSlideActivo
            in
            ( { model
                | cualSlideActivo = nuevoSlideActivoValidado
                , aminar = Entra
                , cambia = 0
              }
            , Effect.none
            , Nothing
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

        AnalyticsUsoMenuLigaExterna queLiga ->
            ( model
            , Analytics.toEffect
                pageUrl
                (Analytics.eventoXReportar queLiga)
                AvisadoAnalytics
            , Nothing
            )


subscriptions : Maybe PageUrl -> RouteParams -> Path -> Shared.Model -> Model -> Sub Msg
subscriptions maybePageUrl routeParams path sharedModel model =
    Sub.none



-- * Data


type alias Data =
    { title : String
    , description : String
    , mainHead : HeaderText
    , beneficios : Beneficios
    , menus : List View.Liga
    , body : Result String (List Markdown.Block.Block)
    }


type alias Beneficios =
    { preHeader : String
    , header : String
    , subHeader : String
    , motivos : List Arts
    }


type alias HeaderText =
    { preMainHeader : String
    , mainHeaderResaltado : String
    , postMainHeader : String
    , mainSubHeader : String
    }


type alias Arts =
    { cabeza : String
    , nota : String
    }


data : DataSource Data
data =
    let
        miDecoder : String -> Decoder Data
        miDecoder elCuerpo =
            Decode.map6 Data
                (Decode.field "title" Decode.string)
                (Decode.field "description" Decode.string)
                (Decode.field "mainHead" mainHeaderDecoder)
                (Decode.field "beneficios" beneDecoder)
                MenuDecoder.decodificaLigas
                -- pausa
                (elCuerpo
                    |> MdConverter.parsea
                    |> Decode.succeed
                )

        beneDecoder =
            Decode.map4 Beneficios
                (Decode.field "preHeader" Decode.string)
                (Decode.field "header" Decode.string)
                (Decode.field "subHeader" Decode.string)
                (Decode.field "art" artsDecoder
                    |> Decode.list
                    |> Decode.field "motivos"
                )

        artsDecoder =
            Decode.map2 Arts
                (Decode.field "cabeza" Decode.string)
                (Decode.field "nota" Decode.string)

        mainHeaderDecoder =
            Decode.map4 HeaderText
                (Decode.field "preMainHeader" Decode.string)
                (Decode.field "mainHeaderResaltado" Decode.string)
                (Decode.field "postMainHeader" Decode.string)
                (Decode.field "mainSubHeader" Decode.string)
    in
    File.bodyWithFrontmatter
        miDecoder
        (HardCodedData.siteName ++ "/index.md")


head : StaticPayload Data ActionData RouteParams -> List Head.Tag
head static =
    let
        logotipo : Seo.Image
        logotipo =
            { url = "logotipo.png" |> Path.fromString |> Pages.Url.fromPath
            , alt = "Sitio oficial de " ++ static.data.title
            , dimensions = Just { width = 800, height = 670 }
            , mimeType = Just <| MimeType.Image MimeType.Png
            }
    in
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = static.sharedData.siteName
        , image = logotipo
        , description = static.data.description
        , locale = HardCodedData.localito
        , title = static.data.title
        }
        |> Seo.website



-- * View
-- ** Del Original


view : Maybe PageUrl -> Shared.Model -> Model -> StaticPayload Data ActionData RouteParams -> View (Pages.Msg.Msg Msg)
view maybeUrl sharedModel model static =
    { title =
        static.data.title
    , body =
        [ div
            []
            [ viewMenu
                (Just Route.Index)
                sharedModel.showMenu
                static.data.menus
                { mainHero = viewHeroMain static.data.mainHead
                , afterHero = viewHeroAfter
                }
            , viewFeatures static.data.beneficios
            , viewNotificacion sharedModel.usuarioStatus model.verNotificaciones
            , viewGaleria model
            , indexViewFooter
            , div
                [ class "tw mt-8 prose prose-headings:font-serif" ]
                (MdConverter.renderea static.data.body)
                |> Html.map (\_ -> Pages.Msg.UserMsg NoOp)
            ]
        ]
    , withMenu = View.NoMenu
    }



-- *** Notificaciones - modals


respFromPost : Result Http.Error String -> String
respFromPost resp =
    case resp of
        Ok _ ->
            "Registrado Ok, nos comunicaremos pronto."

        Err cualError ->
            ErroresHttp.viewHttpError cualError


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



-- ** Del Shared


viewMenu : Maybe Route -> Bool -> List View.Liga -> { mainHero : Html (Pages.Msg.Msg Msg), afterHero : Html (Pages.Msg.Msg Msg) } -> Html (Pages.Msg.Msg Msg)
viewMenu ruta menuOpen ligas complementos =
    let
        quePagina : String
        quePagina =
            ruta
                |> Maybe.map Route.routeToPath
                |> Maybe.withDefault [ "pagina-rara" ]
                |> List.foldr String.append ""

        quePaginaCompuesta : String
        quePaginaCompuesta =
            "pag-index"

        clasesMenuItems : ( Bool, Bool ) -> Html.Attribute msg
        clasesMenuItems ( esMovil, especial ) =
            case ( esMovil, especial ) of
                ( True, True ) ->
                    class "tw block w-full px-5 py-3 text-center font-medium text-blue-900 bg-gray-50 hover:bg-gray-200"

                ( True, False ) ->
                    class "tw block px-3 py-2 rounded-md text-base font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50"

                ( False, True ) ->
                    class "tw font-medium text-blue-900 hover:text-blue-500"

                ( False, False ) ->
                    class "tw font-medium text-gray-500 hover:text-gray-900"

        menuItem : Bool -> View.Liga -> Html (Pages.Msg.Msg Msg)
        menuItem esMovil laLiga =
            case laLiga.dir of
                View.Otra camino ->
                    Html.a
                        [ Attr.href <| Path.toRelative camino
                        , clasesMenuItems ( esMovil, laLiga.especial )
                        , camino
                            |> Path.toSegments
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault "-ligaexterna-rara-"
                            |> String.append (quePaginaCompuesta ++ "-menuliga-externa-")
                            |> AnalyticsUsoMenuLigaExterna
                            |> Pages.Msg.UserMsg
                            |> Event.onClick
                        ]
                        [ text laLiga.queDice ]

                View.Interna rutaLiga ->
                    rutaLiga
                        |> Route.link
                            [ clasesMenuItems ( esMovil, laLiga.especial ) ]
                            [ text laLiga.queDice ]

        showMovilMenu : Bool -> Animation
        showMovilMenu show =
            if show then
                Animation.fromTo
                    { duration = 180
                    , options = [ Animation.easeOut ]
                    }
                    [ P.opacity 0, P.scale 0.9 ]
                    [ P.opacity 1, P.scale 1 ]

            else
                Animation.fromTo
                    { duration = 125
                    , options = [ Animation.easeIn ]
                    }
                    [ P.opacity 1, P.scale 1 ]
                    [ P.opacity 0, P.scale 0.9 ]

        movilMenu lasLigas =
            div
                [ class "tw z-20 rounded-lg shadow-md bg-white ring-1 ring-black ring-opacity-5" ]
                [ div
                    [ class "tw px-5 pt-4 flex items-center justify-between" ]
                    [ Html.img
                        [ class "h-8 w-auto"
                        , Attr.src <|
                            MiCloudinary.url "f_auto" "v1634944374/logo-psolar2_nrh1xt.svg"
                        , Attr.alt "logo PSOLAR.MX"
                        ]
                        []
                    , div
                        [ class "tw -mr-2" ]
                        [ Html.button
                            [ Attr.type_ "button"
                            , class "tw bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                            , Event.onClick (Pages.Msg.UserMsg ToggleMenu)
                            ]
                            [ Html.span
                                [ class "tw sr-only" ]
                                [ text "Close main menu" ]
                            , HeroIcons.outlineX
                            ]
                        ]
                    ]
                , div
                    [ class "tw px-2 pt-2 pb-3 space-y-1" ]
                    (List.map
                        (menuItem True)
                        lasLigas
                    )
                ]

        corteDiagonal =
            Svg.svg
                [ SvgAttr.class "tw hidden lg:block absolute right-0 inset-y-0 h-full w-48 text-white transform translate-x-1/2"
                , SvgAttr.fill "currentColor"
                , SvgAttr.viewBox "0 0 100 100"
                , SvgAttr.preserveAspectRatio "none"
                , Attr.attribute "aria-hidden" "true"
                ]
                [ Svg.polygon
                    [ SvgAttr.points "50,0 100,0 50,100 0,100" ]
                    []
                ]
    in
    div
        [ class "tw relative bg-white" ]
        [ div
            [ class "tw max-w-7xl mx-auto" ]
            [ div
                [ class <|
                    "tw relative z-10 pb-8 bg-white "
                        ++ (if ruta == Just Route.Index then
                                "tw sm:pb-16 md:pb-20 xl:pb-32 lg:max-w-2xl lg:w-full lg:pb-28"

                            else
                                "tw lg:w-full"
                           )
                ]
                [ corteDiagonal
                , div []
                    [ div
                        [ class "tw relative pt-6 px-4 sm:px-6 lg:px-8" ]
                        [ Html.nav
                            [ class "tw relative flex items-center justify-between sm:h-10 lg:justify-start"
                            , Attr.attribute "aria-label" "Global"
                            ]
                            [ div
                                [ class "tw flex items-center flex-grow flex-shrink-0 lg:flex-grow-0" ]
                                [ div
                                    [ class "tw flex items-center justify-between w-full md:w-auto" ]
                                    [ Html.img
                                        [ class "tw h-8 w-auto sm:h-10"
                                        , Attr.src <|
                                            MiCloudinary.url "f_auto" "v1634944374/logo-psolar2_nrh1xt.svg"
                                        , Attr.alt "logo PSOLAR.MX"
                                        ]
                                        []
                                    , div
                                        [ class "tw -mr-2 flex items-center md:hidden" ]
                                        [ Html.button
                                            [ Attr.type_ "button"
                                            , class "tw bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                            , Attr.attribute "aria-expanded" "false"
                                            , Event.onClick (Pages.Msg.UserMsg ToggleMenu)
                                            ]
                                            [ Html.span
                                                [ class "tw sr-only" ]
                                                [ text "Abrir menu principal" ]
                                            , HeroIcons.outlineMenu
                                            ]
                                        ]
                                    ]
                                ]
                            , div
                                [ class "tw hidden md:block md:ml-10 md:pr-4 md:space-x-8" ]
                                (List.map
                                    (menuItem False)
                                    ligas
                                )
                            , if menuOpen then
                                -- Animated.div
                                --    (showMovilMenu menuOpen)
                                div
                                    [ class "tw absolute z-20 top-0 inset-x-0 origin-top-right md:hidden" ]
                                    [ movilMenu ligas ]

                              else
                                div [] []
                            ]
                        ]
                    ]
                , complementos.mainHero
                ]
            ]
        , complementos.afterHero
        ]



-- *** Notificación
-- Footer


indexViewFooter : Html msg
indexViewFooter =
    let
        viewPieNavega : List (Html msg)
        viewPieNavega =
            []

        {-
           [ Footer.ligaAlPie "#" "About"
           , Footer.ligaAlPie "#" "Blog"
           , Footer.ligaAlPie "#" "Jobs"
           , Footer.ligaAlPie "#" "Press"
           , Footer.ligaAlPie "#" "Accesibility"
           , Footer.ligaAlPie "#" "Partners"
           ]
        -}
        viewPieSocialIcons : List (Html msg)
        viewPieSocialIcons =
            [ Footer.ligaIcono "https://github.com/rolojf/psolar" "GitHub" Footer.Github
            , Footer.ligaIcono "https://www.linkedin.com/in/rolando-flores-gzz-80887163/" "LinkedIn" Footer.LinkedIn

            --, Footer.ligaIcono "whatsapp.com" "Whatsapp" Footer.WhatsApp
            , Footer.ligaIcono
                (Route.Contacto |> Route.toPath |> Path.toRelative)
                "Correo"
                Footer.Email
            ]
    in
    Footer.viewFooter
        viewPieNavega
        viewPieSocialIcons
        "REFTEX INGENIERIA, S.A. de C.V. - 2022"



-- Above the Fold


viewHeroMain headText =
    Html.main_
        [ class "tw mt-10 mx-auto max-w-7xl px-4 sm:mt-12 sm:px-6 md:mt-16 lg:mt-20 lg:px-8 xl:mt-28" ]
        [ div
            [ class "tw sm:text-center lg:text-left"
            ]
            [ Html.h1
                [ class "tw text-4xl font-serif font-extrabold text-gray-900 sm:text-5xl" ]
                [ Html.span
                    [ class "tw block xl:inline" ]
                    [ text headText.preMainHeader ]
                , Html.span
                    [ class "tw block text-blue-900 xl:inline" ]
                    [ text <| " " ++ headText.mainHeaderResaltado ++ " " ]
                , Html.span
                    [ class "tw block xl:inline" ]
                    [ text headText.postMainHeader ]
                ]
            , Html.p
                [ class "tw mt-3 text-base text-gray-500 sm:mt-5 sm:text-lg sm:max-w-xl sm:mx-auto md:mt-5 md:text-xl lg:mx-0" ]
                [ text headText.mainSubHeader ]
            , div
                [ class "tw mt-5 sm:mt-8 sm:flex sm:justify-center lg:justify-start" ]
                [ div
                    [ class "tw rounded-md shadow" ]
                    [ Route.Contacto
                        |> Route.link
                            [ class "tw w-full flex items-center justify-center px-8 py-3 border border-transparent text-base font-medium rounded-md text-white bg-blue-900 hover:bg-indigo-700 md:py-4 md:text-lg md:px-10" ]
                            [ text "¡Contáctanos!" ]
                    ]
                , div
                    [ class "tw mt-3 sm:mt-0 sm:ml-3" ]
                    [ Html.a
                        [ Attr.href "#features"
                        , class "tw w-full flex items-center justify-center px-8 py-3 border border-transparent text-base font-medium rounded-md text-indigo-700 bg-indigo-100 hover:bg-indigo-200 md:py-4 md:text-lg md:px-10"
                        ]
                        [ text "más info." ]
                    ]
                ]
            ]
        ]


viewHeroAfter : Html msg
viewHeroAfter =
    div
        [ class "tw lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2" ]
        [ Html.img
            [ class "tw h-56 w-full object-cover object-top sm:h-72 md:h-96 lg:w-full lg:h-full lg:object-right"
            , Attr.src <| MiCloudinary.url "f_auto,q_auto:best" "dreamstime_s_30697263_clymr0.jpg"
            , Attr.alt "Expertos trajamos por ti"
            ]
            []
        ]



-- View Features


viewFeatures : Beneficios -> Html msg
viewFeatures bene =
    let
        viewArts : Arts -> Html msg
        viewArts articulo =
            div
                [ class "tw relative" ]
                [ Html.dt []
                    [ HeroIcons.outlineCheck
                    , Html.p
                        [ class "tw ml-9 text-lg leading-6 font-medium text-gray-900"
                        ]
                        [ text articulo.cabeza ]
                    ]
                , Html.dd
                    [ class "tw mt-2 ml-9 text-base text-gray-500"
                    ]
                    [ text articulo.nota ]
                ]
    in
    div
        [ class "tw bg-white"
        , Attr.id "features"
        ]
        [ div
            [ class "tw max-w-7xl mx-auto py-16 px-4 sm:px-6 lg:py-24 lg:px-8 lg:grid lg:grid-cols-3 lg:gap-x-8" ]
            [ div []
                [ Html.h2
                    [ class "tw text-base font-semibold text-indigo-600 uppercase tracking-wide" ]
                    [ text bene.preHeader ]
                , Html.p
                    [ class "tw mt-2 text-3xl font-extrabold text-gray-900 font-serif tracking-wide" ]
                    [ text bene.header ]
                , Html.p
                    [ class "tw mt-4 text-lg text-gray-500" ]
                    [ text bene.subHeader ]
                ]
            , div
                [ class "tw mt-12 lg:mt-0 lg:col-span-2" ]
                [ Html.dl
                    [ class "tw space-y-10 sm:space-y-0 sm:grid sm:grid-cols-2 sm:grid-rows-3 sm:grid-flow-col sm:gap-x-6 sm:gap-y-10 lg:gap-x-8" ]
                    (List.map viewArts bene.motivos)
                ]
            ]
        ]



-- ** View Galeria


textosGal : Array String
textosGal =
    [ "Uno"
    , "Dos"
    , "tres"
    , "cuatro"
    , "cinco"
    ]
        |> Array.fromList


viewGaleria : Model -> Html (Pages.Msg.Msg Msg)
viewGaleria modeloDeGal =
    let
        listadoCompletoImgs : Array String
        listadoCompletoImgs =
            List.map
                (\cual -> "https://picsum.photos/seed/" ++ String.fromChar cual ++ "/700/700")
                (String.toList "abcdefghijklmnopqrst")
                |> Array.fromList
    in
    viewGal
        listadoCompletoImgs
        textosGal
        modeloDeGal


viewGal : Array String -> Array String -> Model -> Html (Pages.Msg.Msg Msg)
viewGal listadoCompletoImgs textos model =
    div
        [ Attr.id "slider-container" ]
        [ viewSlider
            model.showSlider
            listadoCompletoImgs
            textos
            model.cualSlideActivo
            model.aminar
        ]


viewSlider : Bool -> Array String -> Array String -> Int -> Amimacion -> Html (Pages.Msg.Msg Msg)
viewSlider showIt listadoCompletoImgs textos slideActivo animar =
    let
        letraVa : Int -> Animation
        letraVa orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (orden * 70)
                    , Animation.easeInQuint
                    ]
                }
                [ P.opacity 1
                , P.y 0
                ]
                [ P.opacity 0
                , P.y -60.0
                ]

        letraViene : Int -> Animation
        letraViene orden =
            Animation.fromTo
                { duration = 600
                , options =
                    [ Animation.delay (1000 + orden * 70)
                    , Animation.easeOutQuint
                    ]
                }
                [ P.opacity 0
                , P.y 60.0
                ]
                [ P.opacity 1
                , P.y 0
                ]

        fotoVa : Int -> Animation
        fotoVa orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (500 + 120 * orden)
                    , Animation.easeInCubic
                    ]
                }
                [ P.opacity 1
                , P.y 0
                ]
                [ P.opacity 0
                , P.y -600.0
                ]

        fotoViene : Int -> Animation
        fotoViene orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (200 * orden)
                    , Animation.easeInCubic
                    ]
                }
                [ P.opacity 0
                , P.y 600
                ]
                [ P.opacity 1
                , P.y 0
                ]

        despliega4 : Array String -> List (Html msg)
        despliega4 subListado =
            Array.toIndexedList subListado
                |> List.foldl
                    -- (a -> b -> b) -> b -> List a -> b -//- tuple  -> Html msg
                    (\( indice, direccion ) listadoAc ->
                        div
                            [ class <| "img img-" ++ String.fromInt (indice + 1) ]
                            [ case animar of
                                Sale ->
                                    Animated.html
                                        Html.img
                                        (fotoVa indice)
                                        [ Attr.src direccion ]
                                        []

                                Entra ->
                                    Animated.html
                                        Html.img
                                        (fotoViene indice)
                                        [ Attr.src direccion ]
                                        []
                            ]
                            :: listadoAc
                    )
                    []

        seccionDeImagenes desdeCual =
            div
                [ class "imgs" ]
                [ div
                    [ class "grid" ]
                  <|
                    despliega4 <|
                        Array.slice
                            desdeCual
                            (desdeCual + 4)
                            listadoCompletoImgs
                ]

        seccionTexto =
            div
                [ class "content" ]
                [ div
                    [ class "wrap" ]
                    (textos
                        |> Array.get slideActivo
                        |> Maybe.withDefault ""
                        |> String.toList
                        |> List.indexedMap
                            (\indice letra ->
                                case animar of
                                    Sale ->
                                        Animated.html
                                            Html.span
                                            (letraVa indice)
                                            [ class "letter" ]
                                            [ text (String.fromChar letra) ]

                                    Entra ->
                                        Animated.html
                                            Html.span
                                            (letraViene indice)
                                            [ class "letter" ]
                                            [ text (String.fromChar letra) ]
                            )
                    )
                ]
    in
    div
        [ class "slider" ]
        [ div
            [ class "nav" ]
            [ div
                [ class "next"
                , Event.onClick (Pages.Msg.UserMsg PresionoBotonDer)
                ]
                []
            , div
                [ class "prev"
                , Event.onClick (Pages.Msg.UserMsg PresionoBotonIzq)
                ]
                []
            , div
                [ class "explore-btn" ]
                [ text "Explore" ]
            , if showIt then
                div
                    [ class "item" ]
                    [ seccionDeImagenes (4 * slideActivo)
                    , seccionTexto
                    ]

              else
                div [ class "item" ]
                    [ seccionTexto
                    ]
            ]
        ]
