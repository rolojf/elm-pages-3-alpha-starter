module Route.About exposing (ActionData, Data, Model, Msg, route)

import Browser.Navigation
import DataSource exposing (DataSource)
import Effect exposing (Effect)
import Head
import Head.Seo as Seo
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (Path)
import RouteBuilder exposing (StatefulRoute, StatelessRoute, StaticPayload)
import Shared
import View exposing (View)


type alias Model =
    {}


type Msg
    = NoOp


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


init :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams ActionData
    -> ( Model, Effect Msg )
init maybePageUrl sharedModel static =
    ( {}, Effect.none )


update :
    PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams ActionData
    -> Msg
    -> Model
    -> ( Model, Effect.Effect Msg )
update pageUrl sharedModel static msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


subscriptions : Maybe PageUrl -> RouteParams -> Path -> Shared.Model -> Model -> Sub Msg
subscriptions maybePageUrl routeParams path sharedModel model =
    Sub.none


type alias Data =
    { description : String
    , title : String
    }


data : DataSource Data
data =
    DataSource.succeed
        { description = "Formulario para enviar dudas, comentarios o retroalimentación y hacer contacto o establecer comunicación con "
        , title = "Información sobre "
        }


head :
    StaticPayload Data ActionData RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = static.sharedData.siteName
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.description ++ static.sharedData.siteName
        , locale = static.sharedData.locale
        , title = static.data.title ++ static.sharedData.nosotros
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> templateModel
    -> StaticPayload Data ActionData RouteParams
    -> View templateMsg
view maybeUrl sharedModel model static =
    View.placeholder "About"
