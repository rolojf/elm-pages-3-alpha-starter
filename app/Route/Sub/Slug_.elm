module Route.Sub.Slug_ exposing (ActionData, Data, Model, Msg, route)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Dict exposing (Dict)
import HardCodedData
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block
import MdConverter
import MenuDecoder
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url as Url
import Parser
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
    { slug : String
    }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRender
        { head = head
        , pages = pages
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


type FileType
    = Md
    | Html_


type alias MDFile =
    { slug : String
    , tipo : FileType
    , filePath : String
    }


allMDFiles : DataSource (List MDFile)
allMDFiles =
    Glob.succeed MDFile
        |> Glob.match (Glob.literal (HardCodedData.siteName ++ "/"))
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".")
        |> Glob.capture
            (Glob.oneOf
                ( ( "md", Md )
                , [ ( "html", Html_ ) ]
                )
            )
        |> Glob.captureFilePath
        |> Glob.toDataSource


pages : DataSource (List RouteParams)
pages =
    allMDFiles
        |> DataSource.map
            (List.map
                (\cadaMDFile -> RouteParams cadaMDFile.slug)
            )


type alias ActionData =
    {}


type TipoDeDoc
    = DelMd (Result String (List Markdown.Block.Block))
    | DelHtml (Result String (List Html.Parser.Node))


type alias Data =
    { body : TipoDeDoc
    , title : String
    , menu : View.MenuInfo (Pages.Msg.Msg Msg)
    , description : String
    }


type alias DataPrev =
    { body : String
    , title : String
    , menu : View.MenuInfo (Pages.Msg.Msg Msg)
    , description : String
    }


data : RouteParams -> DataSource Data
data routeParams =
    let
        parsearSegunTipo cualTipo texto =
            case cualTipo of
                Md ->
                    DelMd (MdConverter.parsea texto)

                Html_ ->
                    DelHtml
                        (Html.Parser.run elTexto
                            |> Result.mapError Parser.deadEndsToString
                        )

        miDecoder elCuerpo =
            Decode.map3 (DataPrev elCuerpo)
                (Decode.field "title" Decode.string)
                (MenuDecoder.opMenuToDecode
                    { mainHero = div [] []
                    , afterHero = div [] []
                    }
                )
                (Decode.field "description" Decode.string)

        sacaPath : String -> List MDFile -> String
        {- regresa el path -}
        sacaPath elSlug listadoArchivos =
            listadoArchivos
                |> List.filter
                    (\unArchivo -> unArchivo.slug == elSlug)
                |> List.head
                |> Maybe.map .filePath
                |> Maybe.withDefault "xxx"

        dsPaginaConFrontmatter =
            allMDFiles
                |> DataSource.andThen
                    (\listadoDePaginas ->
                        File.bodyWithFrontmatter
                            miDecoder
                            (sacaPath routeParams.slug listadoDePaginas)
                    )

        sacaElTipo : String -> List MDFile -> FileType
        sacaElTipo elSlug listadoArchivos =
            listadoArchivos
                |> List.filter
                    (\unArchivo -> unArchivo.slug == elSlug)
                |> List.head
                |> Maybe.map .tipo
                |> Maybe.withDefault Html_

        dsTipoDePagina =
            allMDFiles
                |> DataSource.map
                    (sacaElTipo routeParams.slug)
    in
    DataSource.map2
        (\dPrev dTipo ->
            { body = parsearSegunTipo dTipo dPrev.body
            , title = dPrev.title
            , menu = dPrev.menu
            , description = dPrev.description
            }
        )
        dsPaginaConFrontmatter
        dsTipoDePagina


head :
    StaticPayload Data ActionData RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = static.sharedData.siteName
        , image =
            { url = Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.description
        , locale = HardCodedData.localito
        , title = static.data.title
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg ())
view maybeUrl sharedModel static =
    { title = static.data.title
    , body =
        [ Html.div
            [ class "tw prose prose-headings:font-serif" ]
            (case static.data.body of
                DelMd cuerpoMd ->
                    MdConverter.renderea cuerpoMd

                DelHtml cuerpoHtml ->
                    case cuerpoHtml of
                        Ok nodos ->
                            Html.Parser.Util.toVirtualDom nodos

                        Err errores ->
                            [ div [] [ text errores ] ]
            )
        ]
    , withMenu =
        -- View.SiMenu ligas { mainHero = div [] [], afterHero = div [] [] }
        static.data.menu
    }
