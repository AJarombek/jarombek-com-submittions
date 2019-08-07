#!/usr/bin/env bash

# Get the kinds of types in Haskell
# Author: Andrew Jarombek
# Date: 7/31/2019

ghci

# :k gets the Kinds of type constructors

:k Maybe
# Maybe :: * -> *

:k []
# [] :: * -> *

:k Either
# Either :: * -> * -> *

:k Functor
# Functor :: (* -> *) -> Constraint

:k Applicative
# Applicative :: (* -> *) -> Constraint