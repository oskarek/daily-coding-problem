{-# LANGUAGE RecordWildCards, TemplateHaskell, NoImplicitPrelude #-}
-- | The solution uses the State monad with an IntMap in it
-- to simulate a memory layout, with access via addresses.

module DailyCodingProblem.Problem6.Solution where

import           Prelude                 hiding ( head
                                                , tail
                                                )
import           Data.Bits
import qualified Data.IntMap                   as IM
import           Control.Monad.State
import           System.Random
import           Control.Lens            hiding ( both )

data Env a = Env { _addrMap :: IM.IntMap (XORNode a)
                 , _gen :: StdGen }

type AllocState a = State (Env a)
type Address = Int

data XORLinkedList a =
    XORLinkedList { _head :: Address, _tail :: Address }
    deriving (Eq, Ord, Show)

data XORNode a = XORNode
    { _value :: a, _both :: Address }
    deriving (Eq, Ord, Show)

makeLenses ''XORLinkedList
makeLenses ''XORNode
makeLenses ''Env

mkLinkedList :: XORLinkedList a
mkLinkedList = XORLinkedList { _head = 0, _tail = 0 }

runAlloc :: AllocState a b -> b
runAlloc = flip evalState emptyEnv where emptyEnv = Env IM.empty (mkStdGen 0)

randomAddress :: AllocState a Address
randomAddress = do
    rGen <- gets (view gen)
    let (addr, newGen) = random rGen
    modify (gen .~ newGen)
    return addr

alloc :: Ord a => XORNode a -> AllocState a Address
alloc xs = do
    addr <- randomAddress
    modify (addrMap %~ IM.insert addr xs)
    return addr

-- | Add an element to the end of the linked list.
add :: Ord a => XORLinkedList a -> a -> AllocState a (XORLinkedList a)
add ll a = do
    addr <- alloc XORNode { _value = a, _both = ll^.tail }
    if ll^.tail == 0
        then return $ XORLinkedList addr addr
        else do
            modify (addrMap %~ IM.adjust (both %~ (`xor` addr)) (ll^.tail))
            return (ll & tail .~ addr)

-- | Try to get the element at the specified index.
(!) :: XORLinkedList a -> Int -> AllocState a (Maybe a)
(!) ll = go (ll^.head) 0
  where
    go nodeAddr prevAddr n
        | nodeAddr == 0 = return Nothing
        | otherwise = do
            node <- derefPointer nodeAddr
            if n == 0
                then return (Just $ node^.value)
                else do
                    let nextAddr = prevAddr `xor` (node^.both)
                    go nextAddr nodeAddr (n - 1)

derefPointer :: Address -> AllocState a (XORNode a)
derefPointer addr = (IM.! fromIntegral addr) <$> gets _addrMap
