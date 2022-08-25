module Site exposing (config)

import DataSource exposing (DataSource)
import Head
import SiteConfig exposing (SiteConfig)
import MimeType
import Pages.Url as Url


config : SiteConfig
config =
    { canonicalUrl = "https://solarpaq.com"
    , head = head
    }


head : DataSource (List Head.Tag)
head =
    [ --Head.sitemapLink "/sitemap.xml"
    Head.icon [(32, 32)] MimeType.Png (Url.external "/favicon.ico")
    , Head.icon [ ] (MimeType.OtherImage "svg+xml") (Url.external "/icon.svg")
    , Head.appleTouchIcon (Just 180) (Url.external "/apple-touch-icon.png")

    ]
        |> DataSource.succeed
