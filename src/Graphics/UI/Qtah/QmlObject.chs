#include "qtahqml.h"
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Graphics.UI.Qtah.QmlObject where

import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Foreign.Hoppy.Runtime 
import Graphics.UI.Qtah.Qml.QJSValue (QJSValue)
import Graphics.UI.Qtah.Core.QObject
import Graphics.UI.Qtah.Core.Unsafe

-- A QmlObject contain a set of method in the type of HsUniformFun
-- each of which reads a QJSValue from QML side, and produces 
-- a QJSValue. If the return value is allocated in Haskell side,
-- then it is better to be moved to GC by toGc. 
type HsUniformFun = Ptr QJSValue -> IO (Ptr QJSValue)

data QmlObject a = QmlObject (Ptr (QmlObject a))
                 | QmlObjectGc (ForeignPtr ()) (Ptr (QmlObject a))

data QmlObjectConst a = QmlObjectConst (Ptr (QmlObjectConst a))
                      | QmlObjectConstGc (ForeignPtr ()) (Ptr (QmlObjectConst a))

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

castQObjectToNonconst :: QmlObjectConst a -> QmlObject a
castQObjectToNonconst (QmlObjectConst ptr') = QmlObject $ castPtr ptr'
castQObjectToNonconst (QmlObjectConstGc fptr' ptr') = QmlObjectGc fptr' $ castPtr ptr'

instance QObjectPtr (QmlObject a) where
  toQObject (QmlObject ptr') = qObjectFromRawPtr $ castPtr $ castQmlObjectToQObject $ castPtr ptr'
  toQObject (QmlObjectGc fptr' ptr') = qObjectFromRawPtrGc fptr' $ castPtr $ castQmlObjectToQObject $ castPtr ptr'

instance QObjectConstPtr (QmlObject a) where
  toQObjectConst = toQObjectConst . toQObject 

instance QObjectConstPtr (QmlObjectConst a) where
  toQObjectConst = toQObjectConst . castQObjectToNonconst

instance Eq (QmlObject a) where
  x == y = toPtr x == toPtr y

instance Ord (QmlObject a) where
  compare x y = compare (toPtr x) (toPtr y)

instance Eq (QmlObjectConst a) where
  x == y = toPtr x == toPtr y

instance Ord (QmlObjectConst a) where
  compare x y = compare (toPtr x) (toPtr y)

instance CppPtr (QmlObject a) where
  nullptr = QmlObject nullPtr
  
  withCppPtr (QmlObject ptr') f' = f' ptr'
  withCppPtr (QmlObjectGc fptr' ptr') f' = withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (QmlObject ptr') = ptr'
  toPtr (QmlObjectGc _ ptr') = ptr'
  
  touchCppPtr (QmlObject _) = return ()
  touchCppPtr (QmlObjectGc fptr' _) = touchForeignPtr fptr'

instance CppPtr (QmlObjectConst a) where
  nullptr = QmlObjectConst nullPtr
  
  withCppPtr (QmlObjectConst ptr') f' = f' ptr'
  withCppPtr (QmlObjectConstGc fptr' ptr') f' = withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (QmlObjectConst ptr') = ptr'
  toPtr (QmlObjectConstGc _ ptr') = ptr'
  
  touchCppPtr (QmlObjectConst _) = return ()
  touchCppPtr (QmlObjectConstGc fptr' _) = touchForeignPtr fptr'

instance Deletable (QmlObject a) where
  delete (QmlObject ptr') = delete'QmlObject $ castPtr ptr'
  delete (QmlObjectGc _ _) = fail $ concat ["Deletable.delete: Asked to delete a GC-managed ", "QmlObject", " object."]
  toGc this'@(QmlObject ptr') = if ptr' == nullPtr then return this' else fmap (flip QmlObjectGc ptr') $ newForeignPtr (castFunPtr deletePtr'QmlObject :: FunPtr (Ptr () -> IO ())) (castPtr ptr' :: Ptr ())
  toGc this'@(QmlObjectGc {}) = return this'

instance Deletable (QmlObjectConst a) where
  delete (QmlObjectConst ptr') = delete'QmlObject ptr'
  delete (QmlObjectConstGc _ _) = fail $ concat ["Deletable.delete: Asked to delete a GC-managed ", "QmlObjectConst", " object."]
  toGc this'@(QmlObjectConst ptr') = if ptr' == nullPtr then return this' else fmap (flip QmlObjectConstGc ptr') $ newForeignPtr (castFunPtr deletePtr'QmlObject :: FunPtr (Ptr () -> IO ())) (castPtr ptr' :: Ptr ())
  toGc this'@(QmlObjectConstGc {}) = return this'  

class (QObjectConstPtr this) => QmlObjectConstPtr this a where
  toQmlObjectConst :: this -> (QmlObjectConst a)

class (QmlObjectConstPtr this a, QObjectPtr this) => QmlObjectPtr this a where
  toQmlObject :: this -> QmlObject a

instance QmlObjectConstPtr (QmlObject a) a where
  toQmlObjectConst (QmlObject ptr') = QmlObjectConst $ castPtr ptr'
  toQmlObjectConst (QmlObjectGc fptr' ptr') = QmlObjectConstGc fptr' $ castPtr ptr'

instance QmlObjectPtr (QmlObject a) a where
  toQmlObject = id

second :: (b -> c) -> (a,b) -> (a,c)
second f (x,y) = (x,f y)

foreign import ccall "wrapper" mkCallback :: HsUniformFun -> IO (FunPtr HsUniformFun) 
foreign import ccall "genpop__QmlObject_new" qQmlObject_new ::   Ptr HsClassInfo -> IO (Ptr (QmlObject a))
foreign import ccall "gencast__QmlObject__QObject" castQmlObjectToQObject :: Ptr (QmlObjectConst a) -> Ptr QObjectConst
foreign import ccall "gencast__QObject__QmlObject" castQObjectToQmlObject :: Ptr QObjectConst -> Ptr (QmlObjectConst a)
foreign import ccall "gendel__QmlObject" delete'QmlObject :: Ptr (QmlObjectConst a) -> IO ()
foreign import ccall "&gendel__QmlObject" deletePtr'QmlObject :: FunPtr (Ptr (QmlObjectConst a) -> IO ())

data HsClassInfo = HsClassInfo {
    hs_name :: CString,
    hs_method_num :: Int,
    hs_methods :: Ptr (FunPtr HsUniformFun),
    hs_methods_names :: Ptr CString
}

instance Storable HsClassInfo where
    alignment _ = {#alignof HsClassInfo#}
    sizeOf    _ = {#sizeof  HsClassInfo#}
    peek _      = error "HsClassInfo cannot peek"
    poke ptr obj = do
      pokeByteOff ptr {#offsetof HsClassInfo->className  #} (hs_name          obj)
      pokeByteOff ptr {#offsetof HsClassInfo->numMethods #} (hs_method_num    obj)
      pokeByteOff ptr {#offsetof HsClassInfo->ptrMethods #} (hs_methods       obj)
      pokeByteOff ptr {#offsetof HsClassInfo->methodsName#} (hs_methods_names obj)      

withHsClassInfo :: String -> [(String, HsUniformFun)] ->
                (Ptr HsClassInfo -> IO a) -> IO a
withHsClassInfo clazz methods action = do
    withCString clazz (\cclazz -> 
        withMany withMethod methods (\np -> do
            let names = map fst np
                fptrs = map snd np
                num   = length  np
            cnames <- newArray names
            cfptrs <- newArray fptrs
            with (HsClassInfo cclazz num cfptrs cnames) action))

  where 
    withMethod :: (String, HsUniformFun) -> ((CString, FunPtr HsUniformFun) -> IO a) -> IO a
    withMethod (n,p) f = withCString n (\cn -> do 
                           cp <- mkCallback p
                           f (cn, cp))
