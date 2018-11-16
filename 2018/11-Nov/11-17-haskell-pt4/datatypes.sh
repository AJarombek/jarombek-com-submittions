#!/usr/bin/env bash

# Observe Haskell types in ghci
# Author: Andrew Jarombek
# Date: 11/15/2018

ghci
:load data

:type Just
# Just :: a -> Maybe a

:type Miles
# Miles :: Float -> Distance

:type Kilometers
# Kilometers :: Float -> Distance

:type Meters
# Meters :: Float -> Distance