{-# LANGUAGE ImplicitParams #-}
module Graphics.UI.Qtah.Internal.Class where

import Control.Monad
import Control.Monad.Trans.Class
import Graphics.UI.Qtah.Qml.QJSValue (QJSValue)
import Graphics.UI.Qtah.Qml.QJSEngine(QJSEngine)

class JS a where
  fromJS :: QJSValue -> IO (Maybe a)
  toJS :: (?engine :: QJSEngine) => a -> IO QJSValue

ensure b = lift b >>= guard