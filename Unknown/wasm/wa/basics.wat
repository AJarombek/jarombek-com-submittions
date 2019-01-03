;; Basic Web Assembly functions
;; Author: Andrew Jarombek
;; Date: 12/31/2018

(module
  ;; Function section
  (type $calc_pace_1a (func (param i32 i32 i32) (result i32)))
  (type $calc_pace_1b (func (param i32 i32 i32) (result i32)))
  (type $calc_pace_2 (func (param f32 i32 i32) (result f32)))
  (type $setInt (func (param i32 i32)))
  (type $getInt (func (param i32) (result i32)))
  (type $inc (func (result i32)))
  (type $div (func (param i32 i32 i32) (result i32)))

  ;; Global section
  (global $stored_location i32 (i32.const 26))

  ;; Memory section
  ;; This module requires one page of memory (64 KiB)
  (memory 1)

  ;; Export section
  (export "calc_pace_1a" (func $calc_pace_1a))
  (export "calc_pace_1b" (func $calc_pace_1b))
  (export "calc_pace_2" (func $calc_pace_2))
  (export "setInt" (func $setInt))
  (export "getInt" (func $getInt))
  (export "inc" (func $inc))
  (export "div" (func $div))

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

  ;; Increment a number each time the function is invoked.
  (func $inc (result i32)
    get_global $stored_location
    call $getInt

    i32.const 1
    i32.add
    set_local $value

    get_global $stored_location
    get_local $value
    call $setInt

    get_local $value
  )

  ;; Perform division on two integers.  If the type parameter is 0, an integer
  ;; division is performed.  Otherwise a floating point division is performed.
  (func $div (param $num i32) (param $den i32) (param $type i32) (result i32)
    (if (result i32)
      (i32.eqz
        (get_local $type)
      )
      (then
        get_local $num
        ;;get_local $den
        ;;i32.div_s
      )
      (else
        get_local $num
        ;;f32.convert_s/i32

        ;;get_local $den
        ;;f32.convert_s/i32

        ;;f32.div
        ;;i32.trunc_s/f32
      )
    )
  )
)
