module API.User (
  create,
  login,
  current,
  update,
) where

import API.Common
import Control.Category ((.))
import Control.Exception.Safe (try)
import qualified Crypto.JWT as JWT
import qualified Database.Sqlite as DB
import qualified Model.User as Model
import qualified Network.HTTP.Types as HTTP
import Relude hiding ((.))
import WebGear.Server

type CreateUserRequest = Wrapped "user" Model.CreateUserPayload
type UserResponse = Wrapped "user" Model.UserRecord

create ::
  StdHandler h App '[JSONBody CreateUserRequest] [RequiredHeader "Content-Type" Text, JSONBody UserResponse, JSONBody ErrorResponse] =>
  JWT.JWK ->
  RequestHandler h req
create jwk =
  withDoc "Create new user" "Create a new user in the store" $
    jsonRequestBody @CreateUserRequest badRequestBody $
      proc request -> do
        result <- createInDB -< request
        case result of
          Left e -> handleDBError -< e
          Right user -> unlinkA . setDescription okDescription . respondJsonA @UserResponse HTTP.ok200 -< Wrapped user
  where
    createInDB = arrM $ \request -> do
      let userPayload = pick @(JSONBody CreateUserRequest) $ from request
      try $ runDBAction $ Model.create jwk (unwrap userPayload)

handleDBError ::
  StdHandler h App '[] [RequiredHeader "Content-Type" Text, JSONBody ErrorResponse] =>
  h DB.SqliteException Response
handleDBError = proc e ->
  if DB.seError e == DB.ErrorConstraint
    then
      unlinkA
        . setDescription (dupDescription "user account")
        . respondJsonA @ErrorResponse HTTP.badRequest400
        -<
          "Another user account exists with these values"
    else unlinkA . respondJsonA @ErrorResponse HTTP.internalServerError500 -< show e

--------------------------------------------------------------------------------

type LoginUserRequest = Wrapped "user" Model.LoginUserPayload

login ::
  StdHandler h App '[JSONBody LoginUserRequest] [RequiredHeader "Content-Type" Text, JSONBody UserResponse, JSONBody ErrorResponse] =>
  JWT.JWK ->
  RequestHandler h req
login jwk =
  withDoc "Authenticate a user" "Authenticate a user and return their record" $
    jsonRequestBody @LoginUserRequest badRequestBody $
      proc request -> do
        result <- checkCreds -< request
        case result of
          Nothing -> unlinkA . setDescription resp403Description . respondJsonA @ErrorResponse HTTP.forbidden403 -< "Invalid credentials"
          Just user -> unlinkA . setDescription okDescription . respondJsonA @UserResponse HTTP.ok200 -< Wrapped user
  where
    checkCreds = arrM $ \request -> do
      let loginPayload = pick @(JSONBody LoginUserRequest) $ from request
      runDBAction $ Model.checkCredentials jwk (unwrap loginPayload)

--------------------------------------------------------------------------------

current ::
  StdHandler h App '[RequiredAuth] [RequiredHeader "Content-Type" Text, JSONBody UserResponse, JSONBody ErrorResponse] =>
  JWT.JWK ->
  RequestHandler h req
current jwk =
  withDoc "Get current user" "Returns the record of authenticated user" $
    requiredTokenAuth jwk $
      proc request -> do
        result <- getAuthUser -< request
        case result of
          Nothing -> unlinkA . setDescription (resp404Description "User") . notFound404 -< ()
          Just user -> unlinkA . setDescription okDescription . respondJsonA @UserResponse HTTP.ok200 -< Wrapped user
  where
    getAuthUser = arrM $ \request -> do
      let userId = pick @RequiredAuth $ from request
      runDBAction $ Model.getByKey jwk userId

--------------------------------------------------------------------------------

type UpdateUserRequest = Wrapped "user" Model.UpdateUserPayload

update ::
  StdHandler h App [RequiredAuth, JSONBody UpdateUserRequest] [RequiredHeader "Content-Type" Text, JSONBody UserResponse, JSONBody ErrorResponse] =>
  JWT.JWK ->
  RequestHandler h req
update jwk =
  withDoc "Update current user" "Update the authenticated user" $
    requiredTokenAuth jwk $
      jsonRequestBody @UpdateUserRequest badRequestBody $
        proc request -> do
          result <- updateUser -< request
          case result of
            Left e -> handleDBError -< e
            Right Nothing -> unlinkA . setDescription (resp404Description "User") . notFound404 -< ()
            Right (Just user) -> unlinkA . setDescription okDescription . respondJsonA @UserResponse HTTP.ok200 -< Wrapped user
  where
    updateUser = arrM $ \request -> do
      let userId = pick @RequiredAuth $ from request
          userPayload = pick @(JSONBody UpdateUserRequest) $ from request
      try $ runDBAction $ Model.update jwk userId (unwrap userPayload)
