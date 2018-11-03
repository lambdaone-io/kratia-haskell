{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Registry where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


type Address = Int

data Member a = Member {
    address :: Address
} deriving (Eq, Show)

data Community a = Community {
    address :: Address
} 


class Registry f a where

    register :: Community a -> a -> f (Member a)

    -- isMember :: Community a -> Member a -> f Bool

    -- load :: Community a -> Member a -> f (Maybe a)


type RegistryState d a = State (Map Address d) a

instance Register (RegistryState d) d where 

    register = undefined