module API.Article (
  create,
  getBySlug,
  update,
  delete,
  list,
  feed,
  favorite,
  unfavorite,
) where

import API.Common
import Control.Category ((.))
import Control.Exception.Safe (try)
import qualified Crypto.JWT as JWT
import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import qualified Database.Sqlite as DB
import qualified Model.Article as Model
import Model.Entities
import qualified Network.HTTP.Types as HTTP
import Relude hiding ((.))
import WebGear.Server hiding (length)

type CreateArticleRequest = Wrapped "article" Model.CreateArticlePayload
type ArticleResponse = Wrapped "article" Model.ArticleRecord

create ::
  StdHandler
    h
    App
    [RequiredAuth, JSONBody CreateArticleRequest]
    [RequiredHeader "Content-Type" Text, JSONBody ArticleResponse, JSONBody ErrorResponse] =>
  JWT.JWK ->
  RequestHandler h req
create jwk =
  withDoc "Create new article" "Add a new article to the store" $
    requiredTokenAuth jwk $
      jsonRequestBody @CreateArticleRequest badRequestBody $
        setDescription "Add a new article" >>> proc request -> do
          result <- createArticle -< request
          case result of
            Left e -> handleDBError -< e
            Right article ->
              unlinkA . setDescription okDescription . respondJsonA @ArticleResponse HTTP.ok200 -< Wrapped article
  where
    createArticle = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          articlePayload = pick @(JSONBody CreateArticleRequest) $ from request
      try $ runDBAction $ Model.create currentUserId (unwrap articlePayload)

handleDBError ::
  StdHandler h App '[] [RequiredHeader "Content-Type" Text, JSONBody ErrorResponse] =>
  h DB.SqliteException Response
handleDBError = proc e ->
  if DB.seError e == DB.ErrorConstraint
    then unlinkA . setDescription (dupDescription "article") . respondJsonA @ErrorResponse HTTP.badRequest400 -< "Article already exists"
    else unlinkA . respondJsonA @ErrorResponse HTTP.internalServerError500 -< show e

--------------------------------------------------------------------------------

getBySlug ::
  ( StdHandler h App '[OptionalAuth] [RequiredHeader "Content-Type" Text, JSONBody ArticleResponse]
  , HasTrait PathVarSlug req
  ) =>
  JWT.JWK ->
  RequestHandler h req
getBySlug jwk =
  withDoc "Retrieve an article" "Fetch an article by its slug" $
    optionalTokenAuth jwk $
      proc request -> do
        maybeArticle <- fetchArticle -< request
        case maybeArticle of
          Nothing ->
            unlinkA . setDescription (resp404Description "Article") . notFound404 -< ()
          Just article ->
            unlinkA . setDescription okDescription . respondJsonA @ArticleResponse HTTP.ok200 -< Wrapped article
  where
    fetchArticle = arrM $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          slug = pick @PathVarSlug $ from request
      runDBAction $ Model.getArticleBySlug maybeCurrentUserId slug

--------------------------------------------------------------------------------

type UpdateArticleRequest = Wrapped "article" Model.UpdateArticlePayload

update ::
  forall h req.
  ( HasTrait PathVarSlug req
  , StdHandler
      h
      App
      [RequiredAuth, JSONBody UpdateArticleRequest]
      [RequiredHeader "Content-Type" Text, JSONBody ArticleResponse, JSONBody ErrorResponse]
  ) =>
  JWT.JWK ->
  RequestHandler h req
update jwk =
  withDoc "Update an article" "Only the author can update their articles" $
    requiredTokenAuth jwk $
      jsonRequestBody @UpdateArticleRequest badRequestBody $
        proc request -> do
          let userId = pick @RequiredAuth $ from request
              updatePayload = pick @(JSONBody UpdateArticleRequest) $ from request
              articleSlug = pick @PathVarSlug $ from request

          fetchResult <- getArticleIdAndAuthor -< articleSlug
          case fetchResult of
            Nothing -> unlinkA . setDescription (resp404Description "Article") . notFound404 -< ()
            Just (articleId, authorId)
              | authorId /= userId ->
                unlinkA . setDescription resp403Description . respondJsonA @ErrorResponse HTTP.forbidden403 -< "Permission denied"
              | otherwise -> do
                updateResult <- updateArticle -< (authorId, articleId, unwrap updatePayload)
                case updateResult of
                  Left e ->
                    handleDBError -< e
                  Right Nothing ->
                    unlinkA . setDescription (resp404Description "Article") . notFound404 -< ()
                  Right (Just article) ->
                    unlinkA . setDescription okDescription . respondJsonA @ArticleResponse HTTP.ok200 -< Wrapped article
  where
    getArticleIdAndAuthor :: h Text (Maybe (Key Article, Key User))
    getArticleIdAndAuthor = arrM $ \slug -> runDBAction (Model.getArticleIdAndAuthorBySlug slug)

    updateArticle :: h (Key User, Key Article, Model.UpdateArticlePayload) (Either DB.SqliteException (Maybe Model.ArticleRecord))
    updateArticle = arrM $ \(authorId, articleId, updatePayload) ->
      try $ runDBAction $ Model.update authorId articleId updatePayload

--------------------------------------------------------------------------------

delete ::
  ( HasTrait PathVarSlug req
  , StdHandler
      h
      App
      '[RequiredAuth]
      [RequiredHeader "Content-Type" Text, JSONBody ErrorResponse]
  ) =>
  JWT.JWK ->
  RequestHandler h req
delete jwk =
  withDoc "Delete an article" "Only the auther can delete their articles" $
    requiredTokenAuth jwk $ proc request -> do
      let currentUserId = pick @RequiredAuth $ from request
          slug = pick @PathVarSlug $ from request
      _ <- deleteArticle -< (currentUserId, slug)
      unlinkA . setDescription okDescription . noContent204 -< ()
  where
    deleteArticle = arrM $ \(currentUserId, slug) ->
      runDBAction $ Model.delete currentUserId slug

--------------------------------------------------------------------------------

data ArticleListResponse = ArticleListResponse
  { articles :: [Model.ArticleRecord]
  , articlesCount :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)

param ::
  forall name val h req.
  ( Get h (OptionalQueryParam name val) Request
  , StdHandler
      h
      App
      '[]
      '[RequiredHeader "Content-Type" Text, JSONBody ErrorResponse]
  ) =>
  Description ->
  Middleware h req (OptionalQueryParam name val : req)
param descr nextHandler = optionalQueryParam badRequestParam $ nextHandler . setDescription descr

list ::
  StdHandler
    h
    App
    [ OptionalAuth
    , OptionalQueryParam "tag" Text
    , OptionalQueryParam "author" Text
    , OptionalQueryParam "favorited" Text
    , OptionalQueryParam "limit" Model.Limit
    , OptionalQueryParam "offset" Model.Offset
    ]
    [RequiredHeader "Content-Type" Text, JSONBody ArticleListResponse, JSONBody ErrorResponse] =>
  JWT.JWK ->
  RequestHandler h req
list jwk =
  withDoc "Search articles" "Filter articles by query parameters" $
    optionalTokenAuth jwk $
      param @"tag" @Text "Article tag to filter" $
        param @"author" @Text "Name of Author" $
          param @"favorited" @Text "Favorited by user" $
            param @"limit" @Model.Limit "Number of articles" $
              param @"offset" @Model.Offset "Offset of articles" $
                proc request -> do
                  articles <- listArticles -< request
                  let resp = ArticleListResponse articles (length articles)
                  unlinkA . setDescription okDescription . respondJsonA @ArticleListResponse HTTP.ok200 -< resp
  where
    listArticles = arrM $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          maybeTag = pick @(OptionalQueryParam "tag" Text) $ from request
          maybeAuthorName = pick @(OptionalQueryParam "author" Text) $ from request
          maybeFavoritedBy = pick @(OptionalQueryParam "favorited" Text) $ from request
          listLimit = fromMaybe 20 $ pick @(OptionalQueryParam "limit" Model.Limit) $ from request
          listOffset = fromMaybe 0 $ pick @(OptionalQueryParam "offset" Model.Offset) $ from request

      runDBAction $ Model.articleList Model.ArticleListInput{..}

--------------------------------------------------------------------------------

feed ::
  StdHandler
    h
    App
    [RequiredAuth, OptionalQueryParam "limit" Model.Limit, OptionalQueryParam "offset" Model.Offset]
    [RequiredHeader "Content-Type" Text, JSONBody ArticleListResponse, JSONBody ErrorResponse] =>
  JWT.JWK ->
  RequestHandler h req
feed jwk =
  withDoc "Feed of articles" "Get a list of article the current user follows" $
    requiredTokenAuth jwk $
      param @"limit" @Model.Limit "Number of articles" $
        param @"offset" @Model.Offset "Offset of articles" $
          proc request -> do
            articles <- getArticles -< request
            let resp = ArticleListResponse articles (length articles)
            unlinkA . setDescription okDescription . respondJsonA @ArticleListResponse HTTP.ok200 -< resp
  where
    getArticles = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          listLimit = fromMaybe 20 $ pick @(OptionalQueryParam "limit" Model.Limit) $ from request
          listOffset = fromMaybe 0 $ pick @(OptionalQueryParam "offset" Model.Offset) $ from request

      runDBAction $ Model.articleFeed Model.ArticleFeedInput{..}

--------------------------------------------------------------------------------

favorite ::
  ( HasTrait PathVarSlug req
  , StdHandler
      h
      App
      '[RequiredAuth]
      [RequiredHeader "Content-Type" Text, JSONBody ArticleResponse, JSONBody ErrorResponse]
  ) =>
  JWT.JWK ->
  RequestHandler h req
favorite jwk =
  withDoc "Favorite article" "Flag an article as favorite" $
    requiredTokenAuth jwk $
      proc request -> do
        let currentUserId = pick @RequiredAuth $ from request
            slug = pick @PathVarSlug $ from request
        result <- doFavorite -< (currentUserId, slug)
        case result of
          Nothing ->
            unlinkA . setDescription (resp404Description "Article") . notFound404 -< ()
          Just article ->
            unlinkA . setDescription okDescription . respondJsonA @ArticleResponse HTTP.ok200 -< Wrapped article
  where
    doFavorite = arrM $ \(userId, slug) ->
      runDBAction $ Model.favorite userId slug

--------------------------------------------------------------------------------

unfavorite ::
  ( HasTrait PathVarSlug req
  , StdHandler
      h
      App
      '[RequiredAuth]
      '[RequiredHeader "Content-Type" Text, JSONBody ArticleResponse, JSONBody ErrorResponse]
  ) =>
  JWT.JWK ->
  RequestHandler h req
unfavorite jwk =
  withDoc "Unfavorite article" "Remove the favorite flag from an article" $
    requiredTokenAuth jwk $
      proc request -> do
        let currentUserId = pick @RequiredAuth $ from request
            slug = pick @PathVarSlug $ from request
        result <- doUnfavorite -< (currentUserId, slug)
        case result of
          Nothing ->
            unlinkA . setDescription (resp404Description "Article") . notFound404 -< ()
          Just article ->
            unlinkA . setDescription okDescription . respondJsonA @ArticleResponse HTTP.ok200 -< Wrapped article
  where
    doUnfavorite = arrM $ \(userId, slug) ->
      runDBAction $ Model.unfavorite userId slug
