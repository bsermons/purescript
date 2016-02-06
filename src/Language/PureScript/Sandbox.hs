{-# LANGUAGE ScopedTypeVariables #-}
module Language.PureScript.Sandbox where

import Language.PureScript
import Language.PureScript.Externs
import Data.Aeson (decode)
import Data.String (fromString)
import Control.Monad.Except
import Control.Monad.Writer.Strict

decodeExterns :: String -> IO (Maybe ExternsFile)
decodeExterns fp = do
  bs <- readFile fp
  pure $ decode (fromString bs)

ext = do
  Just r <- decodeExterns "externs.json"
  pure r

qualifiedFromString :: String -> String -> Qualified Ident
qualifiedFromString s i = Qualified (Just (moduleNameFromString s)) (Ident i)

myMain = do
  e <- ext
  env <-pure $ applyExternsFileToEnvironment e initEnvironment
  -- print env
  let cs = emptyCheckState env
  -- print cs
  let r = fst . runWriterT $ runExceptT $ evalSupplyT 0 $ do
        [desugared] <- desugar [e] []
        runCheck' env $ typeCheckModule desugared
  print (snd r :: Environment)
