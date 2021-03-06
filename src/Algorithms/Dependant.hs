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

Let's resolve a simple dependency graph:

@
  A <- B     D
  ^    ^
  \    /
     C
@

>>> resolve [("B", ["A"]), ("A", []), ("C", ["B", "A"]), ("D", [])]
Right [("A",[]),("D",[]),("B",["A"]),("C",["B","A"])]

Now let's try resolving a cyclic dependency:

@
   _____
  /     \
  |     v
  A     B     D <- C
  ^     |
  \     /
   -----
@

>>> resolve [("B", ["A"]), ("A", ["B"]), ("C", ["D"]), ("D", [])]
Left ["B","A"]

You can use the 'DependantPlus' typeclass if you want to express dependencies
in the other direction as well. Example:

@
data Example = Example { eIdentifier :: String
                       , eBefore     :: [String]
                       , eBehind     :: [String]
                       }
                       deriving (Show)

instance (Dependant String) Example where
    identifier = eIdentifier
    dependencies = eBehind

instance (DependantPlus String) Example where
    dependencyRequests = eBefore
@

Let's try using 'resolvePlus' (second and first are from Data.Bifunctor):

>>> second (map identifier) $ resolvePlus [Example "a" [] ["c", "d"], Example "b" ["c"] ["d"], Example "c" [] [], Example "d" [] []]
Right ["d","b","c","a"]

>>> first (map identifier) $ resolvePlus [Example "a" ["b"] ["b"], Example "b" [] ["c"], Example "c" [] []]
Left ["a","b"]

-}

module Algorithms.Dependant
    ( resolve
    , Dependant(..)
    , resolvePlus
    , DependantPlus(..)
    ) where

import Data.Either () -- for the Bifunctor instance
import Data.Bifunctor (bimap)
import Data.List (partition)
import Control.Monad (forM, liftM)

-- | Typeclass for all topologically sortable things
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

-- | Check whether all dependencies of a Dependant are in a given list.
resolvable :: (Dependant id a, Eq id)
           => [a]  -- ^ The list in which to look for the dependencies
           -> a    -- ^ The Dependant whose dependencies shall be looked for
           -> Bool -- ^ Whether all the dependencies are in the list
resolvable xs x = all (`elem` map identifier xs) (dependencies x)

-- | Typeclass for topologically sortable things that declare dependencies in
--   the other direction as well
class DependantPlus id a | a -> id where
    dependencyRequests :: a -> [id] -- ^ Returns items that are requested to
                                    --   depend on the given item

-- | A helper type that is used to add extra dependencies to a Dependant
data Wrap id a = Wrap a [id]

instance (Dependant id a) => Dependant id (Wrap id a) where
    identifier (Wrap item _) = identifier item
    dependencies (Wrap item extraIds) = extraIds ++ (dependencies item)

-- | Try to topologically sort a list of 'DependantPlus' items
resolvePlus :: (Dependant id a, DependantPlus id a, Eq id)
            => [a]
            -> Either [a] [a]
resolvePlus xs = mapBoth (map restore) . resolve . map addDepends $ xs
    where addDepends x = Wrap x . dependencyRequesters xs $ identifier x
          restore (Wrap x _) = x

-- | Pick the 'DependantPlus' items that declare a dependency request on a
--   given Dependant.
dependencyRequesters :: (Dependant id a, DependantPlus id a, Eq id)
                     => [a]  -- ^ The list of 'DependantPlus' items to be checked
                     -> id   -- ^ The Dependant id on which an item must declare
                             --   a dependency request in order to end up in the
                             --   result list
                     -> [id] -- ^ The list of all identifiers whose items
                             --   declare the dependency request
dependencyRequesters ds d = map identifier $ filter declares ds
    where declares dp = d `elem` dependencyRequests dp

mapBoth :: (a -> b) -> Either a a -> Either b b
mapBoth f e = bimap f f e
