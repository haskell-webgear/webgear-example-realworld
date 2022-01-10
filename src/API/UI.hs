module API.UI where

import API.Common (App)
import Network.Mime (MimeType)
import Relude
import WebGear.Server

assets ::
  StdHandler h App '[] [RequiredHeader "Content-Type" MimeType, Body LByteString] =>
  RequestHandler h req
assets = serveDir "ui/assets" Nothing

index ::
  StdHandler h App '[] [RequiredHeader "Content-Type" MimeType, Body LByteString] =>
  RequestHandler h req
index = proc _ -> serveFile -< "ui/index.html"
