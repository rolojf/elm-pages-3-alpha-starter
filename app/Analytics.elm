module Analytics exposing (Event, eventoXReportar, none, toCmd)

import Effect exposing (Effect)
import Http



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


toCmd : Event -> (Result Http.Error () -> msg) -> Effect msg
toCmd event msg =
    case event of
        Event cualEvento ->
            Effect.SoloAccedeLiga
                ("https://psolar.mx/api-v1/msg/"
                    ++ cualEvento
                )
                msg

        None ->
            Effect.none
