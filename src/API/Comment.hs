module API.Comment (
  create,
  list,
  API.Comment.delete,
) where

import API.Common
import Control.Category ((.))
import qualified Crypto.JWT as JWT
import qualified Database.Persist.Sql as DB
import qualified Model.Comment as Model
import qualified Network.HTTP.Types as HTTP
import Relude hiding (Set, (.))
import WebGear.Server

type CreateCommentRequest = Wrapped "comment" Model.CreateCommentPayload
type CommentResponse = Wrapped "comment" Model.CommentRecord

type PathVarCommentId = PathVar "commentId" Int64

create ::
  ( StdHandler
      h
      App
      [RequiredAuth, JSONBody CreateCommentRequest]
      [RequiredHeader "Content-Type" Text, JSONBody CommentResponse, JSONBody ErrorResponse]
  , HasTrait PathVarSlug req
  ) =>
  JWT.JWK ->
  RequestHandler h req
create jwk =
  withDoc "Add a new comment" "Add a comment to an article" $
    requiredTokenAuth jwk $
      jsonRequestBody @CreateCommentRequest badRequestBody $
        proc request -> do
          maybeComment <- createComment -< request
          case maybeComment of
            Nothing -> unlinkA . setDescription (resp404Description "Comment") . notFound404 -< ()
            Just comment -> unlinkA . setDescription okDescription . respondJsonA @CommentResponse HTTP.ok200 -< Wrapped comment
  where
    createComment = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @PathVarSlug $ from request
          payload = pick @(JSONBody CreateCommentRequest) $ from request
      runDBAction $ Model.create currentUserId slug (unwrap payload)

--------------------------------------------------------------------------------

type CommentListResponse = Wrapped "comments" [Model.CommentRecord]

list ::
  ( StdHandler
      h
      App
      '[OptionalAuth]
      [RequiredHeader "Content-Type" Text, RequiredHeader "Content-Type" Text, JSONBody CommentListResponse, JSONBody ErrorResponse]
  , HasTrait PathVarSlug req
  ) =>
  JWT.JWK ->
  RequestHandler h req
list jwk =
  withDoc "List comments" "List all comments of an article" $
    optionalTokenAuth jwk $
      proc request -> do
        comments <- listComments -< request
        unlinkA . setDescription okDescription . respondJsonA @CommentListResponse HTTP.ok200 -< Wrapped comments
  where
    listComments = arrM $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          slug = pick @PathVarSlug $ from request
      runDBAction $ Model.list maybeCurrentUserId slug

--------------------------------------------------------------------------------

delete ::
  ( StdHandler
      h
      App
      '[RequiredAuth]
      [RequiredHeader "Content-Type" Text, JSONBody ErrorResponse]
  , HaveTraits [PathVarSlug, PathVarCommentId] req
  ) =>
  JWT.JWK ->
  RequestHandler h req
delete jwk =
  withDoc "Delete a comment" "Only an author can delete their comments" $
    requiredTokenAuth jwk $
      deleteComment
        >>> unlinkA . setDescription okDescription . noContent204
  where
    deleteComment = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @PathVarSlug $ from request
          commentId = pick @PathVarCommentId $ from request
      runDBAction $ Model.delete currentUserId slug (DB.toSqlKey commentId)
