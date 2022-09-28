module Site exposing (canonicalUrl, config)

import DataSource exposing (DataSource)
import HardCodedData
import Head
import MimeType
import Pages.Url as Url
import SiteConfig exposing (SiteConfig)


canonicalUrl : String
canonicalUrl =
    HardCodedData.canonicalUrl


config : SiteConfig
config =
    { canonicalUrl = canonicalUrl
    , head = head
    }


head : DataSource (List Head.Tag)
head =
    [ --Head.sitemapLink "/sitemap.xml"
      Head.icon [ ( 32, 32 ) ] MimeType.Png (Url.external "/favicon.ico")
    , Head.icon [] (MimeType.OtherImage "svg+xml") (Url.external "/icon.svg")
    , Head.appleTouchIcon (Just 180) (Url.external "/apple-touch-icon.png")
    ]
        |> DataSource.succeed
