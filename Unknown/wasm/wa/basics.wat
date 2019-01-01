;; Basic Web Assembly functions
;; Author: Andrew Jarombek
;; Date: 12/31/2018

(func $calc_pace (param $miles f32) (param $minutes i32) (param $seconds i32)
  get_local $minutes
  i32.const 60
  i32.mul
  get_local $seconds
  i32.add
  get_local $miles
  f32.div
)

(export "calc_pace" (func $calc_pace))
