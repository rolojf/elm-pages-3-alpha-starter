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
    { body : String, title : String, menu : View.MenuInfo Msg }


data : RouteParams -> DataSource Data
data routeParams =
    let
        decodeSegunTipoLiga seaExterna laDireccion =
            if seaExterna then
                Decode.succeed
                    (laDireccion |> Path.fromString |> View.Otra)

            else
                case Route.urlToRoute { path = laDireccion } of
                    Just cual ->
                        Decode.succeed (View.Interna cual)

                    Nothing ->
                        Decode.fail <|
                            "Error no predefinida la ruta con la direccion interna: "
                                ++ laDireccion

        -- ++ " y la path: "
        -- ++ path
        decodificaDireccion : Bool -> Decoder View.LigaTipo
        decodificaDireccion siEsExterna =
            Decode.field "dir" Decode.string
                |> Decode.andThen (decodeSegunTipoLiga siEsExterna)

        decodificaLiga : Decoder View.LigaTipo
        decodificaLiga =
            Decode.field "externa" Decode.bool
                |> Decode.andThen decodificaDireccion

        ligasDecoder : Decoder (List View.Liga)
        ligasDecoder =
            Decode.list
                (Decode.map3
                    View.Liga
                    decodificaLiga
                    (Decode.field
                        "queDice"
                        Decode.string
                    )
                    (Decode.field
                        "especial"
                        Decode.bool
                    )
                )

        miDecoder : String -> Decoder ContenidoConDatos
        miDecoder elCuerpo =
            Decode.map2
                (ContenidoConDatos elCuerpo)
                -- MdConverter.renderea elCuerpo)
                (Decode.field "title" Decode.string)
                (Decode.field
                    "menu"
                    (Decode.map2
                        View.SiMenu
                        ligasDecoder
                        (Decode.succeed
                            { mainHero = div [] []
                            , afterHero = div [] []
                            }
                        )
                    )
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


head : StaticPayload Data RouteParams -> List Head.Tag
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


view : Maybe PageUrl -> Shared.Model -> StaticPayload Data RouteParams -> View Msg
view maybeUrl sharedModel static =
    { title = static.data.delMD.title
    , body =
        [ Html.div
            [ class "prose" ]
            (MdConverter.renderea static.data.delMD.body)
        ]
    , withMenu =
        -- View.SiMenu ligas { mainHero = div [] [], afterHero = div [] [] }
        static.data.delMD.menu
    }
