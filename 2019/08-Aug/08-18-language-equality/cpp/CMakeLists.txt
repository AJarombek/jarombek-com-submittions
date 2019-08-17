# Make the C++ project to test for type equality.
# Author: Andrew Jarombek
# Date: 8/17/2019

cmake_minimum_required(VERSION 3.9)
project(cpp)

set(CMAKE_CXX_STANDARD 17)

add_executable(cpp WrappingPaper.h equality.cpp WrappingPaper.cpp)