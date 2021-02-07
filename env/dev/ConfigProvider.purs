module ConfigProvider where

import Bible.Api.Request (BaseURL(..))

provide :: { baseUrl :: BaseURL, bibleApiUrl :: BaseURL }
provide =
  { baseUrl: BaseURL "http://localhost:8081"
  , bibleApiUrl: BaseURL "http://localhost:5000"
  }
