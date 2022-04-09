module Route.Index exposing (Data, Model, Msg, route)

import DataSource exposing (DataSource)
import DataSource.File as File
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Json.Decode as Decode exposing (Decoder)
import MenuDecoder
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (Path)
import Route exposing (Route)
import RouteBuilder exposing (StatelessRoute, StaticPayload)
import Shared
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { title : String
    , menu : View.MenuInfo Msg
    }


route : StatelessRoute RouteParams Data
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


data : DataSource Data
data =
    let
        miDecoder : Decoder Data
        miDecoder =
            Decode.map2 Data
                (Decode.field "title" Decode.string)
                (MenuDecoder.opMenuToDecode
                    { mainHero = div [] []
                    , afterHero = div [] []
                    }
                )
    in
    File.onlyFrontmatter
        miDecoder
        "content/index.md"


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> Path.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Welcome to elm-pages!"
        , locale = Nothing
        , title = "elm-pages is running"
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = static.data.title
    , body =
        [ Html.h1 [] [ Html.text "elm-pages is up and running!" ]
        , Html.p []
            [ Html.text "The message dale duro"
            ]
        , Route.Blog__Slug_ { slug = "hola" }
            |> Route.link [] [ Html.text "My blog post" ]
        ]
    , withMenu =
        static.data.menu
    }
