{-# LANGUAGE ImplicitParams, TemplateHaskell #-}
module Graphics.UI.Qtah.Internal.JS (
    module Graphics.UI.Qtah.Internal.Class
) where

import qualified Graphics.UI.Qtah.Core.QString as QString 
import qualified Graphics.UI.Qtah.Qml.QJSValue as QJSValue
import qualified Graphics.UI.Qtah.Qml.QJSEngine as QJSEngine
import Foreign.Hoppy.Runtime
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Graphics.UI.Qtah.Internal.Class
import Graphics.UI.Qtah.Internal.Template

instance JS () where
  fromJS a = runMaybeT $ do
    ensure (QJSValue.isNull a)
    return ()
  toJS _ = QJSValue.newFromSpecialValue QJSValue.QJSValueSpecialValue_NullValue

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

instance (JS a1, JS a2) => JS (a1,a2) where
  fromJS = $(fJ 2)
  toJS   = let e = ?engine in $(tJ 2 "e") 

instance (JS a1, JS a2, JS a3) => JS (a1,a2,a3) where
  fromJS = $(fJ 3)
  toJS   = let e = ?engine in $(tJ 3 "e") 

instance (JS a1, JS a2, JS a3, JS a4) => JS (a1,a2,a3,a4) where
  fromJS = $(fJ 4)
  toJS   = let e = ?engine in $(tJ 4 "e") 