{-# LANGUAGE TemplateHaskell, ImplicitParams #-}

module Graphics.UI.Qtah.Internal.Template where

import Language.Haskell.TH
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Graphics.UI.Qtah.Core.QString as QString 
import qualified Graphics.UI.Qtah.Qml.QJSValue as QJSValue
import qualified Graphics.UI.Qtah.Qml.QJSEngine as QJSEngine
import Foreign.Hoppy.Runtime
import Graphics.UI.Qtah.Internal.Class

mkTupleJS :: Int -> Q Dec
mkTupleJS n = do
  names <- sequence $ replicate n (newName "a")
  let cxt = mapM (\n -> [t|JS $(varT n)|]) names
      dec = [t|JS $(return $ foldl AppT (TupleT n) (map VarT names)) |]
      bdy = []  
      --fJS = fmap head 
      --      [d| fromJS a = runMaybeT $ do
      --               ensure (QJSValue.isObject a)
      --               idx1 <- lift $ QString.encode "_1"
      --               idx2 <- lift $ QString.encode "_2"
      --               ensure (QJSValue.hasProperty a idx1)
      --               ensure (QJSValue.hasProperty a idx2)
      --               a1 <- (lift $ QJSValue.property a idx1) >>= MaybeT . fromJS
      --               a2 <- (lift $ QJSValue.property a idx2) >>= MaybeT . fromJS
      --               return (a1,a2) |]

      --tJS = fmap head
      --      [d| toJS (a1,a2) = do
      --              obj <- QJSEngine.newObject ?engine
      --              idx1 <- QString.encode "_1"
      --              idx2 <- QString.encode "_2"
      --              toJS a1 >>= QJSValue.setProperty obj idx1 
      --              toJS a2 >>= QJSValue.setProperty obj idx2
      --              toGc obj |]
  instanceD cxt dec bdy

fJ :: Int -> Q Exp
fJ n = 
    [| \a -> runMaybeT $ do
         ensure (QJSValue.isObject a)
         $(do
             -- construct a list of expressions in the form:
             --   idxi <- lift $ QString.encode "_i"
             --   ensure (QJSValue.hasProperty a idxi)
             --   (lift $ QJSValue.property a idxi) >>= MaybeT . fromJS
             -- finally collect all results as a tuple
             es <- forM [1..n] (\i ->
                       let fi = "_" ++ show i
                       in [| do idx <- lift $ QString.encode fi
                                ensure (QJSValue.hasProperty a idx)
                                (lift $ QJSValue.property a idx) >>= MaybeT . fromJS |])
             tupled (map return es)) |] 

tJ :: Int -> String -> Q Exp
tJ n evar = do
    as <- sequence $ replicate n (newName "a")  
    [| \ $(tupP (map varP as)) -> do
            obj <- QJSEngine.newObject $(varE (mkName evar))
            _ <- $(do
                -- construct a list of expressions in the form:
                --    idx <- QString.encode "_i"
                --    val <- toJS $(ai)
                --    QJSValue.setProperty obj idx val
                es <- forM (zip [1..n] as) (\(i,a) -> 
                          let ai = varE a
                              fi = "_" ++ show i
                          in [| do idx <- QString.encode fi
                                   val <- toJS $(ai) 
                                   QJSValue.setProperty obj idx val
                             |]) 
                listed (map return es))
            toGc obj|]

collect :: ([ExpQ] -> ExpQ) -> [ExpQ] -> ExpQ
collect cl es = do
    let n = length es 
    as <- sequence (replicate n (newName "x"))
    let ret :: ExpQ
        ret = appE (varE $ mkName "return") (cl $ map varE as)
        bind :: (Name, ExpQ) -> ExpQ -> ExpQ
        bind (a,e) r = appE (appE (varE $ mkName ">>=") e) (lam1E (varP a) r)
    foldr bind ret $ zip as es

tupled :: [ExpQ] -> ExpQ
tupled = collect tupE

listed :: [ExpQ] -> ExpQ
listed = collect listE