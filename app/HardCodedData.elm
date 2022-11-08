module HardCodedData exposing (..)

import LanguageTag.Country
import LanguageTag.Language


dataModContacto : { description : String, title : String }
dataModContacto =
    { description = "Formulario para enviar comentarios, retroalimentar o establecer comunicación con " ++ siteName
    , title = "Formato para comunicarse con PSOLAR.MX"
    }



-- Para Site.elm


canonicalUrl : String
canonicalUrl =
    "https://psolar.mx/"



-- imagen a modificar manualmente para cada sitio en módulo contacto


imagen :
    { logoTrans : String
    , logoResource : String
    , altMenuLogo : String
    }
imagen =
    { logoTrans = "f_auto"
    , logoResource = "v1642824483/logoMod_mryxdq.png"
    , altMenuLogo = "nada"
    }



{- HARDCODED Site Info para Shared.elm module
   La idea es que el contenido para cada página este en un folder diferente
   que se define con siteName el nombre del folder
-}


siteName : String
siteName =
    "psolar"


localito : Maybe ( LanguageTag.Language.Language, LanguageTag.Country.Country )
localito =
    Just
        ( LanguageTag.Language.es
        , LanguageTag.Country.mx
        )
