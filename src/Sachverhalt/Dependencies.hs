{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{-| dependant is a simple dependency resolving library.

To be able to use dependant, the items you want to sort have to be an instance
of the 'Dependant' typeclass. You can then call 'resolve' with a list of items
to perform a topological sort.

Example:
@
instance (Dependant String) (String, [String]) where
    identifier = fst
    dependencies = snd
@

@
  A <- B     D
  ^    ^
  \    /
     C
@

Let's resolve a simple dependency graph:

>>> resolve [("B", ["A"]), ("A", []), ("C", ["B", "A"]), ("D", [])]
Right [("A",[]),("D",[]),("B",["A"]),("C",["B","A"])]

@
   _____
  /     \
  |     v
  A     B     D <- C
  ^     |
  \     /
   -----
@

Now let's try resolving a cyclic dependency:

>>> resolve [("B", ["A"]), ("A", ["B"]), ("C", ["D"]), ("D", [])]
Left [("B",["A"]),("A",["B"])]
-}

module Sachverhalt.Dependencies
    ( resolve
    , Dependant
    ) where

import Data.List (partition)
import Control.Monad (forM, liftM)

class Dependant id a | a -> id where
    identifier :: a -> id     -- ^ Returns the unique identifier of an item
    dependencies :: a -> [id] -- ^ Returns the dependencies of an item

-- | Try to topologically sort a list of Dependants.
resolve :: (Dependant id a, Eq id)
        => [a]            -- ^ The list of Dependants
        -> Either [a] [a] -- ^ Return Left with the Dependants that couldn't be
                          --   resolved (cyclic dependency, missing dependency)
resolve xs = go xs []
    where go [] xs = Right xs
          go unresolved resolved =
            let (resolvables, unresolvables) =
                    partition (resolvable resolved) unresolved
            in if not . null $ resolvables
                then go unresolvables (resolved ++ resolvables)
                else Left unresolvables

resolvable :: (Dependant id a, Eq id) => [a] -> a -> Bool
resolvable xs x = all (`elem` map identifier xs) (dependencies x)

class DependantPlus id a | a -> id where
    dependencyRequests :: a -> [id] -- ^ Returns items that are requested to depend on the given item

data Wrap id a = Wrap a [id]

instance (Dependant id a) => Dependant id (Wrap id a) where
    identifier (Wrap item _) = identifier item
    dependencies (Wrap item extraIds) = extraIds ++ (dependencies item)

-- | Try to topologically sort a list of DependantPlus items
resolvePlus :: (Dependant id a, DependantPlus id a, Eq id)
            => [a]
            -> Either [a] [a]

resolvePlus xs = mapEither (map restore) . resolve . map addDepends $ xs
    where addDepends x = Wrap x $ depends x
          depends x = map identifier $ filter (any (== identifier x) . dependencyRequests) xs
          restore (Wrap x _) = x

mapEither :: (a -> b) -> Either a a -> Either b b
mapEither f (Left x) = Left (f x)
mapEither f (Right x) = Right (f x)
