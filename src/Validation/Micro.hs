{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}

{- |
Copyright:  (c) 2014 Chris Allen
            (c) 2014 Edward Kmett
            (c) 2018-2023 Kowainik
            (c) 2023 Marco Zocca, UnfoldML
SPDX-License-Identifier: BSD-3-Clause
Maintainer:  oss@unfoldml.com
Stability:   Stable
Portability: Portable

Lightweight pure data validation based on 'Applicative' functors.

'Validation' allows to accumulate all errors instead of
short-circuting on the first error so you can display all possible
errors at once.

Common use-cases include:

1. Validating each input of a form with multiple inputs.
2. Performing multiple validations of a single value.

'Validation' provides __modular__ and __composable__ interface which
means that you can implement validations for different pieces of your
data independently, and then combine smaller parts into the validation
of a bigger type. The below table illustrates main ways to combine two
'Validation's:

+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+
|   Typeclass   | Operation ○ | 'Failure' e ○ 'Failure' d | 'Success' a ○ 'Success' b | 'Failure' e ○ 'Success' a | 'Success' a ○ 'Failure' e |
+===============+=============+===========================+===========================+===========================+===========================+
| 'Semigroup'   | '<>'        | 'Failure' (e '<>' d)      | 'Success' (a '<>' b)      | 'Failure' e               | 'Failure' e               |
+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+
| 'Applicative' | '<*>'       | 'Failure' (e '<>' d)      | 'Success' (a b)           | 'Failure' e               | 'Failure' e               |
+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+
| 'Alternative' | '<|>'       | 'Failure' (e '<>' d)      | 'Success' a               | 'Success' a               | 'Success' a               |
+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+


In other words, instances of different standard typeclasses provide
various semantics which can be useful in different use-cases:

1. 'Semigroup': accumulate both 'Failure' and 'Success' with '<>'.
2. 'Monoid': 'Success' that stores 'mempty'.
3. 'Functor': change the type inside 'Success'.
4. 'Bifunctor': change both 'Failure' and 'Success'.
5. 'Applicative': apply function to values inside 'Success' and accumulate
   errors inside 'Failure'.
6. 'Alternative': return the first 'Success' or accumulate all errors
   inside 'Failure'.
-}

module Validation.Micro (
  -- * Type
         Validation (..)

         -- * How to use
         -- $use

         -- * Interface functions
       , isFailure
       , isSuccess
       , validation
       , failures
       , successes
       , partitionValidations
       , fromFailure
       , fromSuccess
       , bindValidation

         -- ** 'NonEmpty' combinators
         -- $nonEmptyCombinators
       , failure
       , failureIf
       , failureUnless

         -- ** 'Either' conversion
         -- $either
       , validationToEither
       , eitherToValidation

         -- * Combinators
       , validateAll

      -- * When* functions
    , whenSuccess
    , whenFailure
    , whenSuccess_
    , whenFailure_
    , whenSuccessM
    , whenFailureM
    , whenSuccessM_
    , whenFailureM_

      -- * 'Maybe' conversion
    , failureToMaybe
    , successToMaybe
    , maybeToFailure
    , maybeToSuccess
  ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Data (Data)
import Control.DeepSeq (NFData, NFData1, NFData2(..))
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty (..))

import GHC.Generics (Generic(..), Generic1(..))

{- | 'Validation' is a sum type for storing either all
validation failures or validation success. Unlike 'Either', which
returns only the first error, 'Validation' accumulates all errors
using the 'Semigroup' typeclass.

Usually type variables in @'Validation' e a@ are used as follows:

* @e@: is a list or set of failure messages or values of some error data type.
* @a@: is some domain type denoting successful validation result.

Some typical use-cases:

* @'Validation' ['String'] User@

    * Either list of 'String' error messages or a validated value of a
      custom @User@ type.

* @'Validation' ('NonEmpty' UserValidationError) User@

    * Similar to previous example, but list of failures guaranteed to
      be non-empty in case of validation failure, and it stores values
      of some custom error type.
-}
data Validation e a
    = Failure e
    -- ^ Validation failure. The @e@ type is supposed to implement the 'Semigroup' instance.
    | Success a
    -- ^ Successful validation result of type @a@.
    deriving stock (Eq, Ord, Show, Generic, Generic1, Data)
    deriving anyclass (NFData, NFData1)

{- | Allows changing the value inside 'Success' with a given function.

__Examples__

>>> fmap (+1) (Success 9)
Success 10
>>> fmap (+1) (Failure ["wrong"])
Failure ["wrong"]
-}
instance Functor (Validation e) where
    -- fmap :: (a -> b) -> Validation e a -> Validation e b
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)
    {-# INLINE fmap #-}

    -- (<$) :: a -> Validation e b -> Validation e a
    x <$ Success _ = Success x
    _ <$ Failure e = Failure e
    {-# INLINE (<$) #-}


{- | 'Semigroup' allows merging multiple 'Validation's into single one
by combining values inside both 'Failure' and 'Success'. The '<>'
operator merges two 'Validation's following the below rules:

1. If both values are 'Failure's, returns a new 'Failure' with
accumulated errors.
2. If both values are 'Success'ful, returns a new 'Success' with
combined success using 'Semigroup' for values inside 'Success'.
3. If one value is 'Failure' and another one is 'Success', then
'Failure' is returned.

__Examples__

>>> success1 = Success [9] :: Validation [String] [Int]
>>> success2 = Success [15] :: Validation [String] [Int]
>>> failure1 = Failure ["WRONG"] :: Validation [String] [Int]
>>> failure2 = Failure ["FAIL"]  :: Validation [String] [Int]

>>> success1 <> success2
Success [9,15]
>>> failure1 <> failure2
Failure ["WRONG","FAIL"]
>>> success1 <> failure1
Failure ["WRONG"]
>>> failure2 <> success1 <> success2 <> failure1
Failure ["FAIL","WRONG"]
-}
instance (Semigroup e, Semigroup a) => Semigroup (Validation e a) where
    -- (<>) :: Validation e a -> Validation e a -> Validation e a
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

{- | @'mempty' :: 'Validation' e a@ is @Success@ which stores
@'mempty' :: a@ to be consistent with the 'Semigroup' instance.

__Examples__

>>> mempty :: Validation String [Bool]
Success []
-}
instance (Semigroup e, Monoid a) => Monoid (Validation e a) where
    mempty = Success mempty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}



{- | This instance is the most important instance for the 'Validation' data
type. It's responsible for the many implementations. And it allows to accumulate
errors while performing validation or combining the results in the applicative
style.

__Examples__

>>> success1 = Success 9 :: Validation [String] Int
>>> success2 = Success 15 :: Validation [String] Int
>>> successF = Success (* 2) :: Validation [String] (Int -> Int)
>>> failure1 = Failure ["WRONG"] :: Validation [String] Int
>>> failure2 = Failure ["FAIL"]  :: Validation [String] Int

>>> successF <*> success1
Success 18
>>> successF <*> failure1
Failure ["WRONG"]
>>> (+) <$> success1 <*> success2
Success 24
>>> (+) <$> failure1 <*> failure2
Failure ["WRONG","FAIL"]
>>> liftA2 (+) success1 failure1
Failure ["WRONG"]
>>> liftA3 (,,) failure1 success1 failure2
Failure ["WRONG","FAIL"]

Implementations of all functions are lazy and they correctly work if some
arguments are not fully evaluated.

>>> failure1 *> failure2
Failure ["WRONG","FAIL"]
>>> isFailure $ failure1 *> failure2
True
>>> epicFail = error "Impossible validation" :: Validation [String] Int
>>> isFailure $ failure1 *> epicFail
True
-}
instance Semigroup e => Applicative (Validation e) where
    -- pure :: a -> Validation e a
    pure = Success
    {-# INLINE pure #-}

    Failure e1 <*> b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    Success _ <*> Failure e = Failure e
    Success f <*> Success a = Success (f a)
    {-# INLINE (<*>) #-}

    Failure e1 *> b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    Success _ *> Failure e = Failure e
    Success _ *> Success b = Success b
    {-# INLINE (*>) #-}

    Failure e1 <* b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    Success _ <* Failure e = Failure e
    Success a <* Success _ = Success a
    {-# INLINE (<*) #-}

    liftA2 _ (Failure e1) b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    liftA2 _ (Success _) (Failure e) = Failure e
    liftA2 f (Success a) (Success b) = Success (f a b)
    {-# INLINE liftA2 #-}

-- | NB Validation is not a monad though
bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b
bindValidation v f = case v of
  Failure e -> Failure e
  Success a -> f a


{- | This instance implements the behaviour when the first 'Success'
is returned. Otherwise all 'Failure's are combined.

__Examples__

>>> success1 = Success [9] :: Validation [String] [Int]
>>> success2 = Success [15] :: Validation [String] [Int]
>>> failure1 = Failure ["WRONG"] :: Validation [String] [Int]
>>> failure2 = Failure ["FAIL"]  :: Validation [String] [Int]

>>> success1 <|> success2
Success [9]
>>> failure1 <|> failure2
Failure ["WRONG","FAIL"]
>>> failure2 <|> success2
Success [15]
-}
instance (Monoid e) => Alternative (Validation e) where
    -- empty :: Validation e a
    empty = Failure mempty
    {-# INLINE empty #-}

    -- (<|>) :: Validation e a -> Validation e a -> Validation e a
    s@Success{} <|> _        = s
    _ <|> s@Success{}        = s
    Failure e <|> Failure e' = Failure (e <> e')
    {-# INLINE (<|>) #-}



{- | 'Foldable' for 'Validation' allows folding values inside 'Success'.

__Examples__

>>> fold (Success [16])
[16]
>>> fold (Failure "WRONG!" :: Validation String [Int])
[]
-}
instance Foldable (Validation e) where
    fold = \case
        Failure _ -> mempty
        Success a -> a
    {-# INLINE fold #-}

    foldMap f = \case
        Failure _ -> mempty
        Success a -> f a
    {-# INLINE foldMap #-}

    foldr f x = \case
        Failure _ -> x
        Success a -> f a x
    {-# INLINE foldr #-}

    foldr' = foldr
    {-# INLINE foldr' #-}

    foldl f x = \case
        Failure _ -> x
        Success a -> f x a
    {-# INLINE foldl #-}

    foldl' = foldl
    {-# INLINE foldl' #-}

    toList = \case
        Failure _ -> []
        Success a -> [a]
    {-# INLINE toList #-}

    null = \case
        Failure _ -> True
        Success _ -> False
    {-# INLINE null #-}

    length = \case
        Failure _ -> 0
        Success _ -> 1
    {-# INLINE length #-}

    elem x = \case
        Failure _ -> False
        Success a -> x == a
    {-# INLINE elem #-}

    sum = \case
        Failure _ -> 0
        Success a -> a
    {-# INLINE sum #-}

    product = \case
        Failure _ -> 1
        Success a -> a
    {-# INLINE product #-}



{- | Traverse values inside 'Success' with some effectful computation.

__Examples__

>>> parseInt = readMaybe :: String -> Maybe Int
>>> traverse parseInt (Success "42")
Just (Success 42)
>>> traverse parseInt (Success "int")
Nothing
>>> traverse parseInt (Failure ["42"])
Just (Failure ["42"])
-}
instance Traversable (Validation e) where
    traverse f (Success a) = Success <$> f a
    traverse _ (Failure e) = pure (Failure e)
    {-# INLINE traverse #-}

    sequenceA = \case
        Failure e -> pure (Failure e)
        Success f -> Success <$> f
    {-# INLINE sequenceA #-}

{- | Similar to 'Functor' but allows mapping of values inside both
'Failure' and 'Success'.

__Examples__

>>> bimap length show (Success 50)
Success "50"
>>> bimap length show (Failure ["15", "9"])
Failure 2
-}
instance Bifunctor Validation where
    bimap f _ (Failure e) = Failure (f e)
    bimap _ g (Success a) = Success (g a)
    {-# INLINE bimap #-}

    first f (Failure e) = Failure (f e)
    first _ (Success a) = Success a
    {-# INLINE first #-}

    second _ (Failure e) = Failure e
    second g (Success a) = Success (g a)
    {-# INLINE second #-}




{- | Similar to 'Foldable' but allows folding both 'Failure' and
'Success' to the same monoidal value according to given functions.

__Examples__

>>> one x = [x]
>>> bifoldMap id (one . show) (Success 15)
["15"]
>>> bifoldMap id (one . show) (Failure ["Wrong", "Fail"])
["Wrong","Fail"]
-}
instance Bifoldable Validation where
    bifoldMap f _ (Failure e) = f e
    bifoldMap _ g (Success a) = g a
    {-# INLINE bifoldMap #-}

{- | Similar to 'Traversable' but traverses both 'Failure' and
'Success' with given effectful computations.

__Examples__

>>> parseInt = readMaybe :: String -> Maybe Int
>>> bitraverse listToMaybe parseInt (Success "42")
Just (Success 42)
>>> bitraverse listToMaybe parseInt (Success "int")
Nothing
>>> bitraverse listToMaybe parseInt (Failure [15])
Just (Failure 15)
>>> bitraverse listToMaybe parseInt (Failure [])
Nothing
-}
instance Bitraversable Validation where
    bitraverse f _ (Failure e) = Failure <$> f e
    bitraverse _ g (Success a) = Success <$> g a
    {-# INLINE bitraverse #-}

instance NFData2 Validation where
    liftRnf2 f _s (Failure x) = f x
    liftRnf2 _f s (Success y) = s y




{- |
== Combinators

We are providing several functions for better integration with the 'Either'
related code in this section.
-}

{- | Transform a 'Validation' into an 'Either'.

>>> validationToEither (Success "whoop")
Right "whoop"

>>> validationToEither (Failure "nahh")
Left "nahh"
-}
validationToEither :: Validation e a -> Either e a
validationToEither = \case
    Failure e -> Left e
    Success a -> Right a
{-# INLINE validationToEither #-}

{- | Transform an 'Either' into a 'Validation'.

>>> eitherToValidation (Right "whoop")
Success "whoop"

>>> eitherToValidation (Left "nahh")
Failure "nahh"
-}
eitherToValidation :: Either e a -> Validation e a
eitherToValidation = \case
    Left e  -> Failure e
    Right a -> Success a
{-# INLINE eitherToValidation #-}



----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

{- | Predicate on if the given 'Validation' is 'Failure'.

>>> isFailure (Failure 'e')
True
>>> isFailure (Success 'a')
False
-}
isFailure :: Validation e a -> Bool
isFailure = \case
    Failure _ -> True
    Success _ -> False

{- | Predicate on if the given 'Validation' is 'Success'.

>>> isSuccess (Success 'a')
True
>>> isSuccess (Failure 'e')
False
-}
isSuccess :: Validation e a -> Bool
isSuccess = \case
    Success _ -> True
    Failure _ -> False

{- | Transforms the value of the given 'Validation' into @x@ using provided
functions that can transform 'Failure' and 'Success' value into the resulting
type respectively.

>>> let myValidation = validation (<> " world!") (show . (* 10))
>>> myValidation (Success 100)
"1000"
>>> myValidation (Failure "Hello")
"Hello world!"
-}
validation :: (e -> x) -> (a -> x) -> Validation e a -> x
validation fe fa = \case
    Success a -> fa a
    Failure e -> fe e

{- | Filters out all 'Failure' values into the new list of @e@s from the given
list of 'Validation's.

Note that the order is preserved.

>>> failures [Failure "Hello", Success 1, Failure "world", Success 2, Failure "!" ]
["Hello","world","!"]
-}
failures :: [Validation e a] -> [e]
failures v = [e | Failure e <- v]
{-# INLINE failures #-}

{- | Filters out all 'Success' values into the new list of @a@s from the given
list of 'Validation's.

Note that the order is preserved.

>>> successes [Failure "Hello", Success 1, Failure "world", Success 2, Failure "!" ]
[1,2]
-}
successes :: [Validation e a] -> [a]
successes v = [a | Success a <- v]
{-# INLINE successes #-}

{- | Redistributes the given list of 'Validation's into two lists of @e@s and
@e@s, where the first list contains all values of 'Failure's and the second
one — 'Success'es correspondingly.

Note that the order is preserved.

>>> partitionValidations [Failure "Hello", Success 1, Failure "world", Success 2, Failure "!" ]
(["Hello","world","!"],[1,2])
-}
partitionValidations :: [Validation e a] -> ([e], [a])
partitionValidations = go
  where
    go :: [Validation e a] -> ([e], [a])
    go []               = ([], [])
    go (Failure e:rest) = first  (e:) $ go rest
    go (Success a:rest) = second (a:) $ go rest

{- | Returns the contents of a 'Failure'-value or a default value otherwise.

>>> fromFailure "default" (Failure "failure")
"failure"
>>> fromFailure "default" (Success 1)
"default"
-}
fromFailure :: e -> Validation e a -> e
fromFailure _ (Failure e) = e
fromFailure e _           = e

{- | Returns the contents of a 'Success'-value or a default value otherwise.

>>> fromSuccess 42 (Success 1)
1
>>> fromSuccess 42 (Failure "failure")
42
-}
fromSuccess :: a -> Validation e a -> a
fromSuccess _ (Success a) = a
fromSuccess a _           = a

----------------------------------------------------------------------------
-- NonEmpty Combinators
----------------------------------------------------------------------------

{- $nonEmptyCombinators

When using 'Validation', we often work with the 'NonEmpty' list of errors, and
those lists will be concatenated later.

The following functions aim to help with writing more concise code.

For example, instead of (perfectly fine) code like:

>>> :{
validateNameVerbose :: String -> Validation (NonEmpty String) String
validateNameVerbose name
    | null name = Failure ("Empty Name" :| [])
    | otherwise = Success name
:}

one can write simply:

>>> :{
validateNameSimple :: String -> Validation (NonEmpty String) String
validateNameSimple name = name <$ failureIf (null name) "Empty Name"
:}

-}

{- | Create a 'Failure' of 'NonEmpty' list with a single given error.

>>> failure "I am a failure"
Failure ("I am a failure" :| [])
-}
failure :: e -> Validation (NonEmpty e) a
failure e = Failure (e :| [])
{-# INLINE failure #-}

{- | Returns a 'Failure' in case of the given predicate is 'True'.
Returns @'Success' ()@ otherwise.

>>> let shouldFail = (==) "I am a failure"
>>> failureIf (shouldFail "I am a failure") "I told you so"
Failure ("I told you so" :| [])
>>> failureIf (shouldFail "I am NOT a failure") "okay"
Success ()
-}
failureIf :: Bool -> e -> Validation (NonEmpty e) ()
failureIf p e
    | p = failure e
    | otherwise = Success ()
{-# INLINE failureIf #-}

{- | Returns a 'Failure' unless the given predicate is 'True'.
Returns @'Success' ()@ in case of the predicate is satisfied.

Similar to 'failureIf' with the reversed predicate.

@
'failureUnless' p ≡ 'failureIf' (not p)
@

>>> let shouldFail = (==) "I am a failure"
>>> failureUnless (shouldFail "I am a failure") "doesn't matter"
Success ()
>>> failureUnless (shouldFail "I am NOT a failure") "I told you so"
Failure ("I told you so" :| [])
-}
failureUnless :: Bool -> e -> Validation (NonEmpty e) ()
failureUnless p e
    | p = Success ()
    | otherwise = failure e
{-# INLINE failureUnless #-}



{- | Validate all given checks in a 'Foldable'. Returns the 'Success' of the
start element when all checks are successful.


A basic example of usage could look like this:

@
> __let__ validatePassword = 'validateAll'
        [ validateEmptyPassword
        , validateShortPassword
        ]

> 'validateAll' \"VeryStrongPassword\"
'Success' \"VeryStrongPassword\"

> 'validateAll' ""
'Failure' (EmptyPassword :| [ShortPassword])
@
-}
validateAll
    :: (Foldable f, Semigroup e)
    => f (a -> Validation e b)
    -> a
    -> Validation e a
validateAll fs a = foldl' (\res f -> res <* f a) (Success a) fs
{-# INLINE validateAll #-}

{- | Applies the given action to 'Validation' if it is 'Failure' and returns the
result. In case of 'Success' the default value is returned.

>>> whenFailure "bar" (Failure 42) (\a -> "foo" <$ print a)
42
"foo"

>>> whenFailure "bar" (Success 42) (\a -> "foo" <$ print a)
"bar"
-}
whenFailure :: Applicative f => x -> Validation e a -> (e -> f x) -> f x
whenFailure _ (Failure e) f = f e
whenFailure a (Success _) _ = pure a
{-# INLINE whenFailure #-}

{- | Applies given action to the 'Validation' content if it is 'Failure'.

Similar to 'whenFailure' but the default value is @()@.

>>> whenFailure_ (Success 42) putStrLn
>>> whenFailure_ (Failure "foo") putStrLn
foo
-}
whenFailure_ :: Applicative f => Validation e a -> (e -> f ()) -> f ()
whenFailure_ = whenFailure ()
{-# INLINE whenFailure_ #-}

{- | Monadic version of 'whenFailure'.
Applies monadic action to the given 'Validation' in case of 'Failure'.
Returns the resulting value, or provided default.

>>> whenFailureM "bar" (pure $ Failure 42) (\a -> "foo" <$ print a)
42
"foo"

>>> whenFailureM "bar" (pure $ Success 42) (\a -> "foo" <$ print a)
"bar"
-}
whenFailureM :: Monad m => x -> m (Validation e a) -> (e -> m x) -> m x
whenFailureM x mv f = mv >>= \v -> whenFailure x v f
{-# INLINE whenFailureM #-}

{- | Monadic version of 'whenFailure_'.
Applies monadic action to the given 'Validation' in case of 'Failure'.
Similar to 'whenFailureM' but the default is @()@.

>>> whenFailureM_ (pure $ Success 42) putStrLn
>>> whenFailureM_ (pure $ Failure "foo") putStrLn
foo
-}
whenFailureM_ :: Monad m => m (Validation e a) -> (e -> m ()) -> m ()
whenFailureM_ mv f = mv >>= \v -> whenFailure_ v f
{-# INLINE whenFailureM_ #-}

{- | Applies the given action to 'Validation' if it is 'Success' and returns the
result. In case of 'Failure' the default value is returned.

>>> whenSuccess "bar" (Failure "foo") (\a -> "success!" <$ print a)
"bar"

>>> whenSuccess "bar" (Success 42) (\a -> "success!" <$ print a)
42
"success!"
-}
whenSuccess :: Applicative f => x -> Validation e a -> (a -> f x) -> f x
whenSuccess x (Failure  _) _ = pure x
whenSuccess _ (Success a) f  = f a
{-# INLINE whenSuccess #-}

{- | Applies given action to the 'Validation' content if it is 'Success'.

Similar to 'whenSuccess' but the default value is @()@.

>>> whenSuccess_ (Failure "foo") print
>>> whenSuccess_ (Success 42) print
42
-}
whenSuccess_ :: Applicative f => Validation e a -> (a -> f ()) -> f ()
whenSuccess_ = whenSuccess ()
{-# INLINE whenSuccess_ #-}

{- | Monadic version of 'whenSuccess'.
Applies monadic action to the given 'Validation' in case of 'Success'.
Returns the resulting value, or provided default.

>>> whenSuccessM "bar" (pure $ Failure "foo") (\a -> "success!" <$ print a)
"bar"

>>> whenSuccessM "bar" (pure $ Success 42) (\a -> "success!" <$ print a)
42
"success!"
-}
whenSuccessM :: Monad m => x -> m (Validation e a) -> (a -> m x) -> m x
whenSuccessM x mv f = mv >>= \v -> whenSuccess x v f
{-# INLINE whenSuccessM #-}

{- | Monadic version of 'whenSuccess_'.
Applies monadic action to the given 'Validation' in case of 'Success'.
Similar to 'whenSuccessM' but the default is @()@.

>>> whenSuccessM_ (pure $ Failure "foo") print
>>> whenSuccessM_ (pure $ Success 42) print
42
-}
whenSuccessM_ :: Monad m => m (Validation e a) -> (a -> m ()) -> m ()
whenSuccessM_ mv f = mv >>= \v -> whenSuccess_ v f
{-# INLINE whenSuccessM_ #-}


{- | Maps 'Failure' of 'Validation' to 'Just'.

>>> failureToMaybe (Failure True)
Just True
>>> failureToMaybe (Success "aba")
Nothing
-}
failureToMaybe :: Validation e a -> Maybe e
failureToMaybe = validation Just (const Nothing)
{-# INLINE failureToMaybe #-}

{- | Maps 'Success' of 'Validation' to 'Just'.

>>> successToMaybe (Failure True)
Nothing
>>> successToMaybe (Success "aba")
Just "aba"
-}
successToMaybe :: Validation e a -> Maybe a
successToMaybe = validation (const Nothing) Just
{-# INLINE successToMaybe #-}

{- | Maps 'Just' to 'Failure' In case of 'Nothing' it wraps the given default
value into 'Success'.

>>> maybeToFailure True (Just "aba")
Failure "aba"
>>> maybeToFailure True Nothing
Success True
-}
maybeToFailure :: a -> Maybe e -> Validation e a
maybeToFailure a = maybe (Success a) Failure
{-# INLINE maybeToFailure #-}

{- | Maps 'Just' to 'Success'. In case of 'Nothing' it wraps the given default
value into 'Failure'

>>> maybeToSuccess True (Just "aba")
Success "aba"
>>> maybeToSuccess True Nothing
Failure True
-}
maybeToSuccess :: e -> Maybe a -> Validation e a
maybeToSuccess e = maybe (Failure e) Success
{-# INLINE maybeToSuccess #-}
