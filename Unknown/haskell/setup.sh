#!/usr/bin/env bash

# Getting started with Haskell
# Author: Andrew Jarombek
# Date: 8/21/2018

# Start Glasgow Haskell Compiler Interface
ghci

# Quit out of Haskell interface
:quit

# Load the file basics.hs
:load basics

# Execute the main function in basics.hs
main

# Inside the GHCI CLI, a really useful feature is to view the types of different functions and values

:type True
# True :: Bool

:type 1
# 1 :: Num p => p

:type 1.4
# 1.4 :: Fractional p => p

:type "Hello"
# "Hello" :: [Char]

:type 'a'
# 'a' :: Char

:type (+)
# (+) :: Num a => a -> a -> a

:type show
# show :: Show a => a -> String