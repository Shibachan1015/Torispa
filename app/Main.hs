-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeOperators #-}

module Main where


main :: IO ()
main = putStrLn "Hello, Haskell!"

--import GHC.TypeLits
--import Web.Scotty
--import Data.Aeson (object, (.=))
--import Control.Monad.IO.Class (liftIO)

-- 自然数を取り扱うための型Natの定義
--data Nat = Z | S Nat deriving (Show, Eq)

-- 自然数をInt型に変換する関数の定義
--fromInt :: Int -> Nat
--fromInt n = if n < 0 then error "Invalid argument" else fromIntegral n

-- 自然数を使って生地を裁断する関数の定義
--cutFabric :: Nat -> Nat -> (Nat, Nat)
--cutFabric n x = (n `divNat` x, n `modNat` x)

-- 自然数を使って横幅を裁断する関数の定義
--cutWidth :: Double -> Double -> Double -> (Nat, Nat)
--cutWidth y z x = cutFabric (fromInt (round (y * x))) (fromInt (round z))

-- 自然数同士の割り算
--divNat :: Nat -> Nat -> Nat
--divNat n m = fst (divModNat n m)

-- 自然数同士の剰余の計算
--modNat :: Nat -> Nat -> Nat
--modNat n m = snd (divModNat n m)

-- 自然数同士の商と余りの計算
--divModNat :: Nat -> Nat -> (Nat, Nat)
--divModNat Z _ = (Z, Z)
--divModNat n m = if n < m then (Z, n) else (S q, r)
--    where (q, r) = divModNat (n `minusNat` m) m

-- 自然数同士の引き算
--minusNat :: Nat -> Nat -> Nat
--minusNat Z _ = Z
--minusNat n Z = n
--minusNat (S n) (S m) = minusNat n m

--main :: IO ()
--main = do
--    scotty 3000 $ do
--        get "/cut-fabric/:n/:x" $ do
--            n <- param "n"
--            x <- param "x"
--            let (quantityX, remainderX) = cutFabric (fromInt n) (fromInt x)
--            json $ object ["quantity" .= quantityX, "remainder" .= remainderX]
--
--        get "/cut-width/:y/:z/:quantityX" $ do
--           y <- param "y"
--            z <- param "z"
--            quantityX <- param "quantityX"
--            let (quantityZ, remainderZ) = cutWidth y z quantityX
--            json $ object ["quantity" .= quantityZ, "remainder" .= remainderZ]






-- {-# LANGUAGE OverloadedStrings #-}

--module Main where

--import Web.Scotty
--import Data.Aeson (object, (.=))
--import Control.Monad.IO.Class (liftIO)

-- 生地を裁断する関数
--cutFabric :: Int -> Int -> (Int, Int)
--cutFabric n x = (n `div` x, n `mod` x)

-- 横幅を裁断する関数
--cutWidth :: Double -> Double -> Double -> (Int, Double)
--cutWidth y z x = cutFabric (y * x) z

--main :: IO ()
--main = do
--    scotty 3000 $ do
--        get "/cut-fabric/:n/:x" $ do
--            n <- param "n"
--            x <- param "x"
--            let (quantityX, remainderX) = cutFabric n x
--            json $ object ["quantity" .= quantityX, "remainder" .= remainderX]

--    get "/cut-width/:y/:z/:quantityX" $ do
--        y <- param "y"
--        z <- param "z"
--        quantityX <- param "quantityX"
--        let (quantityZ, remainderZ) = cutWidth y z quantityX :: (Int, Double)
--        json $ object ["quantity" .= quantityZ, "remainder" .= remainderZ]
--        liftIO $ return ()


