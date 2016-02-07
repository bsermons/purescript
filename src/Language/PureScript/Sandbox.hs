{-# LANGUAGE ScopedTypeVariables #-}
module Language.PureScript.Sandbox where

import           Control.Monad.Except
import           Control.Monad.Writer.Strict
import           Data.Aeson                  (decode)
import           Data.Maybe
import           Data.String                 (fromString)
import           Language.PureScript
import           Language.PureScript.Externs
import           Prelude                     hiding (lex)
import           Text.Parsec                 (eof)

import Debug.Trace
decodeExterns :: String -> IO ExternsFile
decodeExterns = fmap (fromJust . decode . fromString) . readFile

{-
module Test where

function :: forall a. a -> a
function x = x

value :: String
value = "Hello"

-}

tknParser :: TokenParser a -> String -> a
tknParser p s =
  case runTokenParser "" (p <* eof) =<< lex "" s of
    Right x -> x
    Left err -> error (show err)

parseModule' :: String -> Module
parseModule' = tknParser parseModule

parseDeclaration' :: String -> Declaration
parseDeclaration' s =
  let unwrapPositioned (PositionedDeclaration _ _ x) = x
      unwrapPositioned x = x
  in unwrapPositioned (tknParser parseDeclaration s)

qualifiedFromString :: String -> String -> Qualified Ident
qualifiedFromString s i = Qualified (Just (moduleNameFromString s)) (Ident i)

myModule :: Module
myModule = parseModule' $ unlines
  -- [ "module TempModule (a) where"
  -- , "import Prelude"
  -- , "import Test"
  -- , "import Prim"
  -- , "a = \"a\""
  -- , "data Ohai = Lol | Cheezburger Int"
  -- , "testThis = Cheezburger 1"
  -- , "works = map (+ 1) [1,2,3]"
  -- ]
  [ "module TempModule where"
  , "f :: forall x. x -> x"
  , "f a = a"
  , "  where wuttafak = a"
  ]

lookupInTempModule e s =let Just (t, _, _) = lookupValue e (qualifiedFromString "TempModule" s) in t

myMain = do
  eFiles <- traverse decodeExterns ["externs.json", "prelude.externs.json"]
  let env = foldl (flip applyExternsFileToEnvironment) initEnvironment eFiles
  r <- runWriterT $ runExceptT $ evalSupplyT 0 $ do
          [desugared] <- desugar eFiles [myModule]
          -- [wat] <- createBindingGroupsModule [desugared]
          -- trace (show wat) (pure ())
          checked <- runCheck' env $ typeCheckModule desugared
          trace (show checked) (pure ())
          pure checked
  pure ()
  -- case (fst r) of
  --   Right (_, newEnv) -> do
  --     putStrLn $ prettyPrintType $ lookupInTempModule newEnv "f"
  --     return newEnv
  --   Left err -> error (show err)
