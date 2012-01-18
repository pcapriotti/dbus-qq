{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import DBus.QuasiQuoter
import DBus.Types
import Data.Int
import qualified Data.Map as Map
import Data.Maybe
import Data.Word
import Test.QuickCheck
import Text.Printf

main :: IO ()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_simple x y = show (x + y) == [dbus| i i -> s |] f x y
  where
    f :: [Variant] -> [Variant]
    f xs = [toVariant . show . sum $ (map (fromJust . fromVariant) xs :: [Int32])]

prop_nospaces x y = show (x + y) == [dbus|ii->s|] f x y
  where
    f :: [Variant] -> [Variant]
    f xs = [toVariant . show . sum $ (map (fromJust . fromVariant) xs :: [Int32])]

prop_spaces x y = show (x + y) == [dbus|     ii->   s      |] f x y
  where
    f :: [Variant] -> [Variant]
    f xs = [toVariant . show . sum $ (map (fromJust . fromVariant) xs :: [Int32])]

prop_functor x y = Just (show (x + y)) == [dbusF| i i -> s |] f x y
  where
    f :: [Variant] -> Maybe [Variant]
    f xs = Just [toVariant . show . sum $ (map (fromJust . fromVariant) xs :: [Int32])]

prop_maps = Map.empty == [dbus| a{su} -> a{on} |] f Map.empty
  where
    f :: [Variant] -> [Variant]
    f _ = [toVariant (Map.empty :: Map.Map ObjectPath Int16)]

prop_tuples x y = show x ++ y == [dbus| (is) -> s |] f (x, y)
  where
    f :: [Variant] -> [Variant]
    f [x] =
      let (n, s) = fromJust (fromVariant x) :: (Int32, String)
      in [toVariant $ show n ++ s]

prop_retvals x = (x, x) == [dbus| i -> ii |] f x
  where
    f :: [Variant] -> [Variant]
    f [x] = [x, x]

prop_values x = x * 2 == [dbus| -> i |] f
  where
    f :: [Variant] -> [Variant]
    f _ = [toVariant (x * 2)]

prop_unit x = () == [dbus| s -> |] f x
  where
    f :: [Variant] -> [Variant]
    f _ = []

tests = [("simple", quickCheck prop_simple)
        ,("functor", quickCheck prop_functor)
        ,("maps", quickCheck prop_maps)
        ,("tuples", quickCheck prop_tuples)
        ,("retvals", quickCheck prop_retvals)
        ,("values", quickCheck prop_values)
        ,("unit", quickCheck prop_unit)
        ,("no spaces", quickCheck prop_nospaces)
        ,("spaces", quickCheck prop_spaces)]
