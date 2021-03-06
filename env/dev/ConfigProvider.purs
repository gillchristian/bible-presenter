module ConfigProvider where

import Bible.Api.Request (BaseURL(..))

provide :: { baseUrl :: BaseURL, bibleApiUrl :: BaseURL }
provide =
  { baseUrl: BaseURL "http://192.168.178.22:8080"
  , bibleApiUrl: BaseURL "http://192.168.178.22:5000"
  }

