module API.Profile (
  getByName,
  follow,
  unfollow,
) where

import API.Common
import Control.Category ((.))
import qualified Crypto.JWT as JWT
import qualified Model.Profile as Model
import qualified Network.HTTP.Types as HTTP
import Relude hiding ((.))
import WebGear.Server

type ProfileResponse = Wrapped "profile" Model.Profile

type PathVarUsername = PathVar "username" Text

getByName ::
  ( HasTrait PathVarUsername req
  , StdHandler
      h
      App
      '[OptionalAuth]
      [RequiredHeader "Content-Type" Text, JSONBody ProfileResponse]
  ) =>
  JWT.JWK ->
  RequestHandler h req
getByName jwk =
  withDoc "Get a user profile" "Get the profile of a user by name" $
    optionalTokenAuth jwk $
      proc request -> do
        maybeProfile <- fetchProfile -< request
        case maybeProfile of
          Nothing -> unlinkA . setDescription (resp404Description "Profile") . notFound404 -< ()
          Just profile -> unlinkA . setDescription okDescription . respondJsonA @ProfileResponse HTTP.ok200 -< Wrapped profile
  where
    fetchProfile = arrM $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          username = pick @PathVarUsername $ from request
      runDBAction $ Model.getByName maybeCurrentUserId username

follow ::
  ( HasTrait PathVarUsername req
  , StdHandler
      h
      App
      '[RequiredAuth]
      [RequiredHeader "Content-Type" Text, JSONBody ProfileResponse, JSONBody ErrorResponse]
  ) =>
  JWT.JWK ->
  RequestHandler h req
follow jwk =
  withDoc "Follow a user" "" $
    requiredTokenAuth jwk $
      proc request -> do
        maybeProfile <- doFollow -< request
        case maybeProfile of
          Nothing -> unlinkA . setDescription (resp404Description "User") . notFound404 -< ()
          Just profile -> unlinkA . setDescription okDescription . respondJsonA @ProfileResponse HTTP.ok200 -< Wrapped profile
  where
    doFollow = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          username = pick @PathVarUsername $ from request
      runDBAction $ Model.follow currentUserId username

unfollow ::
  ( HasTrait PathVarUsername req
  , StdHandler
      h
      App
      '[RequiredAuth]
      [RequiredHeader "Content-Type" Text, JSONBody ProfileResponse, JSONBody ErrorResponse]
  ) =>
  JWT.JWK ->
  RequestHandler h req
unfollow jwk =
  withDoc "Unfollow a user" "" $
    requiredTokenAuth jwk $
      proc request -> do
        maybeProfile <- doUnfollow -< request
        case maybeProfile of
          Nothing -> unlinkA <<< setDescription (resp404Description "User") . notFound404 -< ()
          Just profile -> unlinkA . setDescription okDescription . respondJsonA @ProfileResponse HTTP.ok200 -< Wrapped profile
  where
    doUnfollow = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          username = pick @PathVarUsername $ from request
      runDBAction $ Model.unfollow currentUserId username
