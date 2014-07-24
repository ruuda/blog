module Site.Configuration (config, feedConfig) where

import Hakyll

ignoreFile' :: String -> Bool
ignoreFile' ".htaccess" = False
ignoreFile' path        = ignoreFile defaultConfiguration path

config :: Configuration
config = defaultConfiguration { ignoreFile = ignoreFile' }

-- Details for the atom feed.
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Ruud van Asseldonk"
  , feedDescription = "Ruud van Asseldonk’s blog"
  , feedAuthorName  = "Ruud van Asseldonk"
  , feedAuthorEmail = "ruudva@veniogames.com"
  , feedRoot        = "http://ruudvanasseldonk.com/"
  }
