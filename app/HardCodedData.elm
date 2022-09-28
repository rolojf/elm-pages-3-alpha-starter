module HardCodedData exposing (..)

import LanguageTag.Country
import LanguageTag.Language


dataModContacto : { description : String, title : String }
dataModContacto =
    { description = "Formulario para enviar dudas, comentarios o retroalimentación y hacer contacto o establecer comunicación con REFTEX INGENIERÍA"
    , title = "Formato para comunicarse con REFTEX INGENIERÍA"
    }



-- Para Site.elm


canonicalUrl : String
canonicalUrl =
    "https://reftex.com/"



-- imagen a modificar manualmente para cada sitio en módulo contacto


imagen :
    { logoTrans : String
    , logoResource : String
    , altMenuLogo : String
    }
imagen =
    { logoTrans = "f_auto"
    , logoResource = "v1619940728/photodune-1522931-hand-holding-tree-for-green-eco-concept-xs_wn4atu.jpg"
    , altMenuLogo = "Trabajemos junto para contribuir para mejorar todos"
    }



{- HARDCODED Site Info para Shared.elm module
   La idea es que el contenido para cada página este en un folder diferente
   que se define con siteName el nombre del folder
-}


siteName : String
siteName =
    "reftex"


localito : Maybe ( LanguageTag.Language.Language, LanguageTag.Country.Country )
localito =
    Just
        ( LanguageTag.Language.es
        , LanguageTag.Country.mx
        )
