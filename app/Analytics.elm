module Analytics exposing (Event, eventoXReportar, none, toEffect)

import Effect exposing (Effect)
import Http
import Path



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


toEffect : String -> Event -> (Result Http.Error () -> msg) -> Effect msg
toEffect host event msg =
    case event of
        Event cualEvento ->
            Effect.SoloAccedeLiga
                ("https:/"
                    ++ ([ host, "api-v1", "msg", cualEvento ]
                            |> Path.join
                            |> Path.toAbsolute
                       )
                )
                msg

        None ->
            Effect.none
