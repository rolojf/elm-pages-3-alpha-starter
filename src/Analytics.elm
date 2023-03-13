module Analytics exposing (Event, eventoXReportar, none, toEffect)

import Effect exposing (Effect)
import Http
import Pages.PageUrl exposing (PageUrl)
import Path
import Url



-------------------------------------------------------------------------------
-- TYPES --
-------------------------------------------------------------------------------


type Event
    = Event String
    | None



-------------------------------------------------------------------------------
-- API --
-------------------------------------------------------------------------------


eventoXReportar : String -> Event
eventoXReportar str =
    Event str


none : Event
none =
    None


toEffect : Event -> (Result Http.Error String -> msg) -> Effect msg
toEffect hayEvento msg =
    case hayEvento of
        Event cualEvento ->
            let
                direccion =
                    [ "api-v2", cualEvento ++ ".json" ]
                        |> Path.join
                        |> Path.toAbsolute
            in
            Effect.SoloAccedeLiga
                direccion
                msg

        None ->
            Effect.none
