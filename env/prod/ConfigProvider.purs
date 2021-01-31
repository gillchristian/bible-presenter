module ConfigProvider where

import Bible.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "https://api.com"
