module Route.Blog.Slug_ exposing (Data, Model, Msg, route)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Json.Decode as Decode exposing (Decoder)
import MdConverter
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import RouteBuilder exposing (StatelessRoute, StaticPayload)
import Shared
import View exposing (View)
import Path exposing (Path)
import Route exposing (Route)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { slug : String }


route : StatelessRoute RouteParams Data
route =
    RouteBuilder.preRender
        { head = head
        , pages = pages
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


type alias MDFile =
    { slug : String
    , filePath : String
    }


allMDFiles : DataSource (List MDFile)
allMDFiles =
    Glob.succeed MDFile
        |> Glob.match (Glob.literal "content/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.captureFilePath
        |> Glob.toDataSource


pages : DataSource (List RouteParams)
pages =
    allMDFiles
        |> DataSource.map
            (List.map
                (\cadaMD -> RouteParams cadaMD.slug)
            )


type alias Data =
    { delMD : ContenidoConDatos }


type alias ContenidoConDatos =
    -- { body : List (Html Msg), title : String }
    { body : String, title : String }


data : RouteParams -> DataSource Data
data routeParams =
    let
        miDecoder : String -> Decoder ContenidoConDatos
        miDecoder elCuerpo =
            Decode.map
                (ContenidoConDatos elCuerpo)
                --MdConverter.renderea elCuerpo)
                (Decode.field "title" Decode.string)

        getDataFromMD =
            File.bodyWithFrontmatter
                miDecoder
                ("content/" ++ routeParams.slug ++ ".md")
    in
    DataSource.map Data
        getDataFromMD


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


view : Maybe PageUrl -> Shared.Model -> StaticPayload Data RouteParams -> View Msg
view maybeUrl sharedModel static =
    { title = static.data.delMD.title
    , body =
        [ Html.div
            [ class "prose" ]
            ( MdConverter.renderea static.data.delMD.body)
        ]
    , withMenu = View.SiMenu ligas { mainHero = div [][], afterHero = div [][] }
    }


ligas : List View.Liga
ligas =
    [ { queDice = "Comunícate"
      , dir = View.Interna Route.Index
      , especial = True
      }
    , { queDice = "Más Información"
      , dir =
            "#features"
                |> Path.fromString
                |> View.Otra
      , especial = False
      }
    ]
