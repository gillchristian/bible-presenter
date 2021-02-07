module ConfigProvider where

import Bible.Api.Request (BaseURL(..))

provide :: { baseUrl :: BaseURL, bibleApiUrl :: BaseURL }
provide =
  { baseUrl: BaseURL "https://api.com"
  , bibleApiUrl: BaseURL "http://localhost:5000"
  }
