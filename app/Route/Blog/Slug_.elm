module Route.Blog.Slug_ exposing (ActionData, Data, Model, Msg, route)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block
import MdConverter
import MenuDecoder
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url as Url
import Path exposing (Path)
import Route exposing (Route)
import RouteBuilder exposing (StatelessRoute, StaticPayload)
import Shared
import Svg exposing (path)
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { slug : String }


route : StatelessRoute RouteParams Data ActionData
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
    { body : Result String (List Markdown.Block.Block)
    , title : String
    , menu : View.MenuInfo (Pages.Msg.Msg Msg)
    }


type alias ActionData =
    {}


data : RouteParams -> DataSource Data
data routeParams =
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
            <|
                "content/"
                    ++ routeParams.slug
                    ++ ".md"
    in
    DataSource.map Data
        getDataFromMD


head :
    StaticPayload Data ActionData RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg ())
view maybeUrl sharedModel static =
    { title = static.data.delMD.title
    , body =
        [ Html.div
            [ class "tw prose" ]
            (MdConverter.renderea static.data.delMD.body)
        ]
    , withMenu =
        -- View.SiMenu ligas { mainHero = div [] [], afterHero = div [] [] }
        static.data.delMD.menu
    }
