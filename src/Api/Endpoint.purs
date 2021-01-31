-- | Bible uses a REST API for resource management.
-- | This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile.
module Bible.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)

data Endpoint
  = BibleAPI

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum {"BibleAPI": noArgs}
