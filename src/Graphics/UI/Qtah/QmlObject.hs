module Graphics.UI.Qtah.QmlObject (
    newQmlObject
  , JS(..)
  , cast
  , castConst
  , Graphics.UI.Qtah.Internal.QmlObject.QmlObjectPtr(..)
  , Graphics.UI.Qtah.Internal.QmlObject.QmlObjectConstPtr(..)
  ) where

import Graphics.UI.Qtah.Internal.QmlObject
import Graphics.UI.Qtah.Internal.JS
import Graphics.UI.Qtah.Qml.QJSValue (QJSValue)
import Foreign.Hoppy.Runtime
import Foreign.Marshal
import Data.IORef


newQmlObject :: a -> [(String, IORef a -> QJSValue -> IO QJSValue)] -> IO (QmlObject a)
newQmlObject self members = do
    refa <- newIORef self
    let memb = map (second (buildCallback refa)) members
    objptr <- withHsClassInfo "QtahQmlObject" memb qQmlObject_new
    return (QmlObject objptr)  
  where
    buildCallback :: IORef a -> (IORef a -> QJSValue -> IO QJSValue) -> HsUniformFun
    buildCallback self fun arg = do
        with arg (\argptr -> do 
          argobj <- decode argptr
          retobj <- fun self argobj
          return $ toPtr retobj)

castConst :: QmlObjectConstPtr this a => this -> (QmlObjectConst a)
castConst = toQmlObjectConst
cast :: QmlObjectPtr this a => this -> (QmlObject a)
cast = toQmlObject