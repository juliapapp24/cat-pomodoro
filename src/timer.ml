open CONSTANTS

type stat = Work | Break | LongBreak
type t = { mutable status : stat; mutable time : int ref }

let work_time = default_work_time
let break_time = default_break_time
let long_break_time = default_long_break_time
let work_obj = { status = Work; time = work_time }
let break_obj = { status = Break; time = break_time }
let long_break_obj = { status = LongBreak; time = long_break_time }
let cycles = ref default_cycles
let cycle_num = ref 1

let change_status timer =
  match timer.status with
  | Work ->
      if !cycle_num < !cycles then (
        timer.status <- Break;
        timer.time <- break_time)
      else (
        timer.status <- LongBreak;
        timer.time <- long_break_time)
  | Break ->
      timer.status <- Work;
      timer.time <- work_time;
      cycle_num := !cycle_num + 1
  | LongBreak ->
      timer.status <- Work;
      timer.time <- work_time;
      cycle_num := 1
