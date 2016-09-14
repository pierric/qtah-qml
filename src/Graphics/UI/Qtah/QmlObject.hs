{-# LANGUAGE ImplicitParams #-}
module Graphics.UI.Qtah.QmlObject (
    newQmlObject
  , JS(..)
  , cast
  , castConst
  , Graphics.UI.Qtah.Internal.QmlObject.QmlObjectPtr(..)
  , Graphics.UI.Qtah.Internal.QmlObject.QmlObjectConstPtr(..)
  ) where

import Graphics.UI.Qtah.Internal.QmlObject
import qualified Graphics.UI.Qtah.Core.QString as QString 
import Graphics.UI.Qtah.Qml.QJSValue (QJSValue)
import qualified Graphics.UI.Qtah.Qml.QJSValue as QJSValue
import Graphics.UI.Qtah.Qml.QJSEngine(QJSEngine)
import qualified Graphics.UI.Qtah.Qml.QJSEngine as QJSEngine
import Foreign.Hoppy.Runtime
import Foreign.Marshal
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
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

class JS a where
  fromJS :: QJSValue -> IO (Maybe a)
  toJS :: (?engine :: QJSEngine) => a -> IO QJSValue

instance JS Int where
  fromJS a = runMaybeT $ do
    ensure (QJSValue.isNumber a)
    lift $ QJSValue.toInt a
  toJS a = QJSValue.newFromInt a >>= toGc

instance JS Bool where
  fromJS a = runMaybeT $ do
    ensure (QJSValue.isBool a)
    lift $ QJSValue.toBool a
  toJS a = QJSValue.newFromBool a >>= toGc

instance JS Double where
  fromJS a = runMaybeT $ do
    ensure (QJSValue.isNumber a)
    lift $ QJSValue.toNumber a
  toJS a = QJSValue.newFromDouble a >>= toGc

instance JS a => JS [a] where
  fromJS a = runMaybeT $ do
    ensure (QJSValue.isArray a)
    lengthProp <- lift $ QString.encode "length"
    num <- lift $ QJSValue.property a lengthProp
    num <- MaybeT $ fromJS num 
    forM ([(0::Int)..num-1]) (\i ->
      lift (QJSValue.propertyWithArrayIndex a (fromIntegral i)) >>= MaybeT . fromJS) 

  toJS as = do
    let num = length as
    arr <- QJSEngine.newArray ?engine num
    forM_ (zip [0..] as) $ \(i,o) -> do
      o' <- toJS o
      QJSValue.setPropertyWithArrayIndex arr i o' 
    toGc arr 

ensure b = lift b >>= guard

