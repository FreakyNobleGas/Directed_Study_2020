-- Name: Nicholas Quinn
--
-- Description: Learning about Monads
--

{-
  (>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b

  The type (>>?) lets us chain together functions that return a Maybe value. So, if 
  the returning type is the same, you could chain Maybe functions indefinitely.

  Unfortunately, this means that if one Maybe function fails, you would have no idea where it
  stopped.


  There are 3 properties that define a Monad in Haskell:

  1) A type constructor m

  2) A function of type m a -> (a -> m b) -> m b for chaining the output of one function into the input
     of another

  3) A function of type a -> m a for injecting a normal value into the chain, i.e. it wraps a type a with the type
     constructor m.

  Example: The properties that make up the textbook's version of the Maybe monad are:

  1) Type Constuctor
  data Maybe a = Nothing
             | Just a

  2) Chaining function (>>?)
  (>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>? _ = Nothing
  Just v  >>? f = f v

  3) Injector Function Just
  inject :: a -> m a

  There is almost no rules on how chaining and injection functions should behave. Injecting is also known as 'binding'

  Monad as defined by the Prelude
  class Monad m where
    -- chain
    (>>=)  :: m a -> (a -> m b) -> m b
    -- inject
    return :: a -> m a

    `Return` returns a pure value (of type a) into a monad (of type m a)

    -- Performs chaining, but ignores value on the left
    (>>) :: m a -> m b -> m b
    a >> f = a >>= \_ -> f

    With >>=
    print "foo" >>= \_ -> print "bar"

    With >>
    print "baz" >> print "quux"

    With >>, we can omit the middle function that serves no purpose in >>= example

    Helpful Jargon:

    - "Monadic" means "pertaining to monads". A monadic type is an instance of the Monad typeclass. A monadic value has a monadic type.
    - When we say that a type "is a monad", it really means that it's an instance of the Monad typeclass.
    - "The Foo monad" is a type called Foo and it's an instance of Monad.
    - An "action" is another name for a monadic value, such as the side effect from the IO monad.

    
-}

module Logger
    (
      Logger
    , Log
    , runLogger
    , record
    ) where

import Control.Applicative
import Control.Monad (liftM, ap)

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])

instance Functor Logger where
    fmap = liftM

instance Applicative Logger where
    pure a = Logger (a, [])
    (<*>) = ap

--(>>=) :: Logger a -> (a -> Logger b) -> Logger b
m >>= k = let 
            (a, w) = execLogger m
            n      = k a
            (b, x) = execLogger n
            in Logger (b, w ++ x)

-- Making a logger
--globToRegex :: String -> Logger String


