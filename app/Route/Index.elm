module Route.Index exposing (Data, Model, Msg, route)

import DataSource exposing (DataSource)
import DataSource.File as File
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block
import MdConverter
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


route : StatelessRoute RouteParams Data
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


type alias Data =
    { delMD : ContenidoConDatos }


type alias ContenidoConDatos =
    { body : Result String (List Markdown.Block.Block)
    , title : String
    , menu : View.MenuInfo Msg
    }


data : DataSource Data
data =
    let
        miDecoder : String -> Decoder ContenidoConDatos
        miDecoder elCuerpo =
            Decode.map3 ContenidoConDatos
                (elCuerpo
                    |> MdConverter.parsea
                    |> Decode.succeed
                )
                (Decode.field "title" Decode.string)
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
    { title = static.data.delMD.title
    , body =
        [ Html.h1 [] [ Html.text "elm-pages is up and running!" ]
        , Html.div
            [ class "tw prose" ]
            (MdConverter.renderea static.data.delMD.body)
        , Route.Blog__Slug_ { slug = "hola" }
            |> Route.link [] [ Html.text "My blog post" ]
        ]
    , withMenu =
        static.data.delMD.menu
    }
