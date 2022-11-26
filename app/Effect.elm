module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)

{-|

@docs Effect, batch, fromCmd, map, none, perform

-}

import Browser.Dom as Dom
import Browser.Navigation
import Form.FormData exposing (FormData)
import Http
import Json.Encode as Encode
import Pages.Fetcher
import Process
import Task
import Url exposing (Url)


{-| -}
type Effect msg
    = None
    | Cmd (Cmd msg)
    | Batch (List (Effect msg))
    | SoloAccedeLiga String (Result Http.Error String -> msg)
    | FetchRouteData
        { data : Maybe FormData
        , toMsg : Result Http.Error Url -> msg
        }
    | PushUrl String
    | Enfoca msg String
    | MandaABasin
        { respuestas : Encode.Value
        , toMsg : Result Http.Error String -> msg
        }
      -- ** De psolar1 Index.elm por Galería
    | EsperaPues Float msg
    | Success msg
    | CheckIfInView String (Result Dom.Error Dom.Element -> msg)



{-
          | GetStargazers (Result Http.Error Int -> msg)
          | SetField { formId : String, name : String, value : String }
          | Submit
              { values : FormData
              , toMsg : Result Http.Error Url -> msg
              }
          | SubmitFetcher (Pages.Fetcher.Fetcher msg)

   type alias RequestInfo =
       { contentType : String
       , body : String
       }
-}


{-| -}
none : Effect msg
none =
    None


{-| -}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| -}
fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


{-| -}
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

        -- * Garlería en Route/Index.elm
        EsperaPues cuantoEsperar msg ->
            EsperaPues cuantoEsperar <| fn msg

        Success msg ->
            Success <| fn msg

        CheckIfInView domElementId msg ->
            CheckIfInView
                domElementId
                (msg >> fn)



{-
   Submit fetchInfo ->
       Submit
           { values = fetchInfo.values
           , toMsg = fetchInfo.toMsg >> fn
           }

   SetField info ->
       SetField info

   SubmitFetcher fetcher ->
       fetcher
           |> Pages.Fetcher.map fn
           |> SubmitFetcher

-}


{-| -}



-- antes: perform ({ fetchRouteData, fromPageMsg, key } as info) effect =


perform :
    { fetchRouteData :
        { data : Maybe FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , submit :
        { values : FormData
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
perform ({ fromPageMsg, key } as helpers) effect =
    case effect of
        None ->
            Cmd.none

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        Batch list ->
            Cmd.batch (List.map (perform helpers) list)

        SoloAccedeLiga direccion toMsg ->
            Http.get
                { url = direccion
                , expect = Http.expectString (toMsg >> fromPageMsg)
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
            helpers.fetchRouteData
                fetchInfo

        -- * De psolar1 Index.elm por Galería
        EsperaPues cuantosMS toMsg ->
            Task.perform
                (\() -> fromPageMsg toMsg)
                (Process.sleep cuantosMS)

        Success toMsg ->
            Task.perform
                (\_ -> fromPageMsg toMsg)
                (Task.succeed ())

        CheckIfInView domElementId toMsg ->
            Task.attempt
                (\elementoResultante -> fromPageMsg (toMsg elementoResultante))
                (Dom.getElement domElementId)



{-
   SetField info ->
       helpers.setField info

   Submit record ->
       helpers.submit record

   SubmitFetcher record ->
       helpers.runFetcher record
-}
