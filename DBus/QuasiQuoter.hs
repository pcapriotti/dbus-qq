module DBus.QuasiQuoter (
  dbus,
  dbusF,
) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Word
import Data.Int
import qualified DBus.Types as DT
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec hiding ((<|>), many)

data DBusFunction = DBusFunction [Type] [Type]

-- |A quasi-quoter to convert a function of type @[Variant] -> [Variant]@ into a
-- function of a specified static type.
--
-- This quasi-quoter takes a signature of the form:
--
-- @
--   \<dbus types\> -> \<dbus types\>
-- @
--
-- Types on the left of the arrow correspond to argument types, while those on
-- the right are return types.
--
-- The result is a combinator which takes any function of type [Variant] ->
-- [Variant], assumes that its arguments and results are of the specified
-- number and types, and returns a function of the corresponding static type.
--
-- For example, if @f :: [Variant] -> [Variant]@,
--
-- @
--   [dbus| i s -> s a{uv} |] f
-- @
--
-- has type
--
-- @
--   Int -> String -> (String, Map Word32 Variant)
-- @
dbus :: QuasiQuoter
dbus = QuasiQuoter
  { quoteExp = expQuoter False
  , quotePat = undefined
  , quoteType = typeQuoter
  , quoteDec = undefined
  }

-- |A generalized version of the dbus quasi-quoter which works on functions of
-- type @[Variant] -> f [Variant]@, for any functor @f@.
dbusF :: QuasiQuoter
dbusF = QuasiQuoter
  { quoteExp = expQuoter True
  , quotePat = undefined
  , quoteType = typeQuoter
  , quoteDec = undefined
  }

expQuoter :: Bool -> String -> Q Exp
expQuoter functor = runQuoter $ \as rs -> do
  f <- newName "f"
  xs <- mapM newName $ replicate (length as) "x"
  result <- thFromVariant functor rs $
    VarE f `AppE` ListE (zipWith thToVariant as xs)
  return . LamE (VarP f : map VarP xs) $ result

typeQuoter :: String -> Q Type
typeQuoter = runQuoter $ \as rs -> return $ thFunc as rs

runQuoter :: ([Type] -> [Type] -> a) -> String -> a
runQuoter f s = case runParser dbusFunction () "" s of
  Left err -> error $ show err
  Right (DBusFunction args rets) -> f args rets

dbusFunction :: GenParser Char s DBusFunction
dbusFunction = DBusFunction
  <$> (space *> dbusTypes <* string "->" <* spaces)
  <*> dbusTypes

dbusTypes :: GenParser Char s [Type]
dbusTypes = many $ dbusType <* spaces

dbusType :: GenParser Char s Type
dbusType =
  (char 'y' *> return (ConT ''Word8)) <|>
  (char 'b' *> return (ConT ''Bool)) <|>
  (char 'n' *> return (ConT ''Int16)) <|>
  (char 'q' *> return (ConT ''Word16)) <|>
  (char 'i' *> return (ConT ''Int32)) <|>
  (char 'u' *> return (ConT ''Word32)) <|>
  (char 'x' *> return (ConT ''Int64)) <|>
  (char 't' *> return (ConT ''Word64)) <|>
  (char 'd' *> return (ConT ''Double)) <|>
  (char 's' *> return (ConT ''String)) <|>
  (char 'o' *> return (ConT ''DT.ObjectPath)) <|>
  (char 'g' *> return (ConT ''DT.Signature)) <|>
  (char 'v' *> return (ConT ''DT.Variant)) <|>
  array <|>
  struct

array :: GenParser Char s Type
array = char 'a' *> (assoc <|> simple)
  where
    assoc = between (char '{') (char '}') $
      AppT <$> (AppT (ConT ''Map.Map) <$> dbusType) <*> dbusType
    simple = AppT ListT <$> dbusType

struct :: GenParser Char s Type
struct =
  between (char '(') (char ')') $ do
    types <- many dbusType
    return $ thStruct types

thToVariant :: Type -> Name -> Exp
thToVariant t name =
  VarE 'DT.toVariant `AppE` (VarE name `SigE` t)

thFromVariant :: Bool -> [Type] -> Exp -> Q Exp
thFromVariant functor ts exp =
  if functor
    then [| fmap $(unpack) $(return exp) |]
    else [| $(unpack) $(return exp) |]
  where
    n = length ts
    convert t = [| \x -> (fromJust $ DT.fromVariant x) :: $(return t) |]
    apply fs = do
      xs <- replicateM n $ newName "x"
      return . LamE [TupP (map VarP xs)] . TupE $
        zipWith AppE fs (map VarE xs)
    unpack = [| $(apply =<< (mapM convert ts)) . $(thTuple n) |]

thStruct :: [Type] -> Type
thStruct ts = foldl AppT (TupleT (length ts)) ts

thTuple :: Int -> ExpQ
thTuple n = do
    ns <- replicateM n (newName "x")
    lamE [foldr (\x y -> conP '(:) [varP x,y]) wildP ns] (tupE $ map varE ns)

thFunc :: [Type] -> [Type] -> Type
thFunc args rets = foldr arr ret args
  where
    arr a b = ArrowT `AppT` a `AppT` b
    ret = thStruct rets
