module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)

import Browser.Navigation
import Http
import Process
import Task
import Url exposing (Url)


type Effect msg
    = None
    | Cmd (Cmd msg)
    | Batch (List (Effect msg))
    | EsperaPues Float msg
    | SoloAccedeLiga String (Result Http.Error () -> msg)
    | FetchPageData
        { body : Maybe { contentType : String, body : String }
        , path : Maybe String
        , toMsg : Result Http.Error Url -> msg
        }


type alias RequestInfo =
    { contentType : String
    , body : String
    }


none : Effect msg
none =
    None


batch : List (Effect msg) -> Effect msg
batch =
    Batch


fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


map : (a -> b) -> Effect a -> Effect b
map fn effect =
    case effect of
        None ->
            None

        Cmd cmd ->
            Cmd (Cmd.map fn cmd)

        Batch list ->
            Batch (List.map (map fn) list)

        SoloAccedeLiga dire toMsg ->
            SoloAccedeLiga dire (toMsg >> fn)

        EsperaPues cuantoEsperar msg ->
            EsperaPues cuantoEsperar <| fn msg

        FetchPageData fetchInfo ->
            FetchPageData
                { body = fetchInfo.body
                , path = fetchInfo.path
                , toMsg = fetchInfo.toMsg >> fn
                }


perform :
    { fetchRouteData :
        { body : Maybe { contentType : String, body : String }
        , path : Maybe String
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg

    --, fromSharedMsg : Shared.Msg -> msg
    , fromPageMsg : pageMsg -> msg
    , key : Browser.Navigation.Key
    }
    -> Effect pageMsg
    -> Cmd msg
perform ({ fetchRouteData, fromPageMsg } as info) effect =
    case effect of
        None ->
            Cmd.none

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        Batch list ->
            Cmd.batch (List.map (perform info) list)

        EsperaPues cuantosMS toMsg ->
            Task.perform
                (\() -> (fromPageMsg toMsg))
                (Process.sleep cuantosMS)

        SoloAccedeLiga direccion toMsg ->
            Http.get
                { url = direccion
                , expect =
                    Http.expectWhatever
                        (toMsg >> fromPageMsg)
                }

        FetchPageData fetchInfo ->
            fetchRouteData
                { body = fetchInfo.body
                , path = fetchInfo.path
                , toMsg = fetchInfo.toMsg
                }
