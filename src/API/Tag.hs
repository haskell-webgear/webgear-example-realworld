module API.Tag (
  list,
) where

import API.Common
import Control.Category ((.))
import qualified Model.Tag as Model
import qualified Network.HTTP.Types as HTTP
import Relude hiding ((.))
import WebGear.Server

type TagsResponse = Wrapped "tags" [Text]

list ::
  StdHandler
    h
    App
    '[]
    [ RequiredHeader "Content-Type" Text
    , JSONBody TagsResponse
    ] =>
  RequestHandler h req
list =
  withDoc "Get all tags" "" $
    proc _request -> do
      tags <- fetchTags -< ()
      unlinkA . setDescription okDescription . respondJsonA @TagsResponse HTTP.ok200 -< Wrapped tags
  where
    fetchTags = arrM $ const $ runDBAction Model.list
