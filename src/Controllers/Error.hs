module Controllers.Error where

import Types
import Web.Scotty.Trans
import Network.HTTP.Types (status400, status404, status500)

handler :: AppError -> ActM ()
handler NotFound = do
  status status404

handler (Unhandled msg) = do
  status status500
  json msg

handler (BadRequest msg) = do
  status status400
  json msg

handler (Unauthorized msg) = do
  status status404
  json msg