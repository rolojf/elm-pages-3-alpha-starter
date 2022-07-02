module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)

import Browser.Dom as Dom
import Browser.Navigation
import FormDecoder
import Http
import Json.Encode as Encode
import Process
import Task
import Json.Decode as Decode
import Pages.Fetcher
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
    | FetchRouteData
        { data : Maybe FormDecoder.FormData
        , toMsg : Result Http.Error Url -> msg
        }
    | PushUrl String
    | Enfoca msg String
    | MandaABasin
        { respuestas : Encode.Value
        , toMsg : Result Http.Error String -> msg
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

        FetchRouteData fetchInfo ->
            FetchRouteData
                { data = fetchInfo.data
                , toMsg = fetchInfo.toMsg >> fn
                }

        PushUrl dire ->
            PushUrl dire

        Enfoca msg queID ->
            Enfoca (fn msg) queID

        MandaABasin someInfo ->
            MandaABasin
                { respuestas = someInfo.respuestas
                , toMsg = someInfo.toMsg >> fn
                }


perform :
    { fetchRouteData :
        { data : Maybe FormDecoder.FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , submit :
        { values : FormDecoder.FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , runFetcher :
        Pages.Fetcher.Fetcher pageMsg
        -> Cmd msg
    , fromPageMsg : pageMsg -> msg
    , key : Browser.Navigation.Key
    , setField : { formId : String, name : String, value : String } -> Cmd msg
    }
    -> Effect pageMsg
    -> Cmd msg
perform ({ fetchRouteData, fromPageMsg, key } as info) effect =
    case effect of
        None ->
            Cmd.none

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        Batch list ->
            Cmd.batch (List.map (perform info) list)

        EsperaPues cuantosMS toMsg ->
            Task.perform
                (\() -> fromPageMsg toMsg)
                (Process.sleep cuantosMS)

        SoloAccedeLiga direccion toMsg ->
            let
                _ =
                    Debug.log "accediendo liga:" direccion
            in
            Cmd.none

        {- Http.get
           { url = direccion
           , expect =
               Http.expectWhatever
                   (toMsg >> fromPageMsg)
           }
        -}
        FetchPageData fetchInfo ->
            fetchRouteData
                { body = fetchInfo.body
                , path = fetchInfo.path
                , toMsg = fetchInfo.toMsg
                }

        PushUrl dir ->
            Browser.Navigation.pushUrl
                key
                dir

        Enfoca toMsg queId ->
            Task.attempt
                (\_ -> fromPageMsg toMsg)
                (Dom.focus queId)

        MandaABasin infoPasada ->
            Http.post
                { url = "https://usebasin.com/f/41489cfac434"
                , body = Http.jsonBody infoPasada.respuestas
                , expect =
                    Http.expectString
                        (infoPasada.toMsg >> fromPageMsg)
                }
        FetchRouteData fetchInfo ->
            info.fetchRouteData fetchInfo
