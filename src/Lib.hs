module Lib
    ( someFunc
    ) where

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Lens
import Control.Applicative 
import Data.Char
import qualified Data.Map as M 
import qualified Data.Set as S 
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Ship = Ship { _numCrew :: Int
                 , _name :: String }

setName :: Ship -> String -> Ship 
setName ship name =
    ship{ _name = name }

name :: Lens' Ship String
name = lens _name setName
