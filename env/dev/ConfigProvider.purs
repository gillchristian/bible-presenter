module ConfigProvider where

import Bible.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "http://localhost:8081"
