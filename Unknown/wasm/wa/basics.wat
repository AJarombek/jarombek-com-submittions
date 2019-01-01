;; Basic Web Assembly functions
;; Author: Andrew Jarombek
;; Date: 12/31/2018

(module
  ;; Function section
  (type $0 (func (param i32 i32 i32) (result i32)))
  (type $1 (func (param i32 i32 i32) (result i32)))
  (type $2 (func (param f32 i32 i32) (result f32)))
  (type $3 (func (param i32 i32)))
  (type $4 (func (param i32) (result i32)))
  (type $5 (func (result i32)))

  ;; Calculate the mile pace of an exercise.  The number of miles exercised must
  ;; be an integer.
  (func $calc_pace_1a (param $miles i32) (param $minutes i32)
                      (param $seconds i32) (result i32)
    get_local $minutes
    i32.const 60
    i32.mul
    get_local $seconds
    i32.add
    get_local $miles
    i32.div_s
  )

  ;; Rewritten with a nested form.
  (func $calc_pace_1b (param $miles i32) (param $minutes i32)
                      (param $seconds i32) (result i32)
    (i32.div_s
      (i32.add
        (i32.mul
          (get_local $minutes)
          (i32.const 60)
        )
        (get_local $seconds)
      )
      (get_local $miles)
    )
  )

  ;; Calculate the mile pace of an exercise.  The number of miles is a floating
  ;; point number.
  (func $calc_pace_2 (param $miles f32) (param $minutes i32)
                      (param $seconds i32) (result f32)
    (f32.div
      ;; Convert the 32-bit integer into a 32-bit floating point number
      (f32.convert_s/i32
        (i32.add
          (i32.mul
            (get_local $minutes)
            (i32.const 60)
          )
          (get_local $seconds)
        )
      )
      (get_local $miles)
    )
  )

  ;; Store an integer at a given location in memory.  The memory location is
  ;; identified by the key parameter.
  (func $setInt (param $key i32) (param $value i32)
    (i32.store
      (get_local $key)
      (get_local $value)
    )
  )

  ;; Retrieve an integer stored at a given location in memory.
  (func $getInt (param $key i32) (result i32)
    (i32.load
      (get_local $key)
    )
  )

  (func $inc (result i32)
    (call $setInt
      ;; Arbitrary memory location
      (i32.const 226)
    )
  )

  (export "calc_pace_1" (func $calc_pace_1a))
  (export "calc_pace_2" (func $calc_pace_2))
  (export "getInt" (func $getInt))
)
