include Graphics
open Unix
open Displayillustration
open Items.AssetDisplay
open CONSTANTS
open Timer
open Grass
open Cat
open Img_gen

type status = Work | Break | LongBreak
type t = { mutable status : status; mutable time : int ref }

let work_time = default_work_time
let break_time = default_break_time
let long_break_time = default_long_break_time
let time_obj = { status = Work; time = work_time }
let cycles = ref default_cycles
let cycle_num = ref 1
let work_color = ref (rgb palette1.work_r palette1.work_g palette1.work_b)
let break_color = ref (rgb palette1.break_r palette1.break_g palette1.break_b)
let long_break_color = ref (rgb palette1.lb_r palette1.lb_g palette1.lb_b)

let flowerbed =
  let op = { default_options with colored = true } in
  let new_tulip = px_array 16 32 [||] true true op in
  let posx, posy =
    (Random.int (windowsizey - gap), Random.int (windowsizex - gap))
  in
  ("reward", (make_image new_tulip, (posx, posy)))

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

let select_string timer =
  match timer.status with
  | Work when !cycle_num = 1 -> "Click to start a work session!"
  | Work -> "Click to start another work session!"
  | Break -> "Click to take a short break!"
  | LongBreak -> "Click to take a long break!"

let emptysafe_int_of_string i = try int_of_string i with Failure e -> 0

let seconds_from_triple (h, m, s) =
  match (h, m, s) with
  | h, m, s ->
      (emptysafe_int_of_string h * 3600)
      + (emptysafe_int_of_string m * 60)
      + emptysafe_int_of_string s

let get_number_input () =
  let input_string = "" in
  let rec get_number_input_rec s =
    match (wait_next_event [ Key_pressed ]).key with
    | c when c = '\r' -> s
    | c
      when c = '0' || c = '1' || c = '2' || c = '3' || c = '4' || c = '5'
           || c = '6' || c = '7' || c = '8' || c = '9' ->
        get_number_input_rec (s ^ string_of_int (int_of_char c - 48))
    | _ -> get_number_input_rec s
  in
  get_number_input_rec input_string

let time_string time =
  (if String.length (string_of_int (!time / 3600)) = 1 then "0" else "")
  ^ string_of_int (!time / 3600)
  ^ (if String.length (string_of_int (!time / 60 mod 60)) = 1 then ":0"
    else ":")
  ^ string_of_int (!time / 60 mod 60)
  ^ (if String.length (string_of_int (!time mod 60)) = 1 then ":0" else ":")
  ^ string_of_int (!time mod 60)

let duration_change_interface time time_unit =
  moveto (windowsizex / 3) (windowsizey - 20);

  draw_string "Configure Settings";
  moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 20);
  draw_string ("Current duration: " ^ time_string time);
  set_color (rgb 255 255 255);
  fill_rect (windowsizex / 10) ((windowsizey / 3 * 2) - 40) 400 15;
  set_color (rgb 0 0 0);
  moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 40);
  draw_string ("Enter a value for " ^ time_unit ^ ", then press [enter].")

let change_time_menu time =
  duration_change_interface time "HOURS";
  let hours = get_number_input () in
  duration_change_interface time "MINUTES";
  let mins = get_number_input () in
  duration_change_interface time "SECONDS";
  let secs = get_number_input () in
  time := seconds_from_triple (hours, mins, secs)

let in_rectangle x y rect_x rect_y rect_w rect_h =
  if x > rect_x && x < rect_x + rect_w && y > rect_y && y < rect_y + rect_h then
    true
  else false

let set_timer_colors palette =
  work_color := rgb palette.work_r palette.work_g palette.work_b;
  break_color := rgb palette.break_r palette.break_g palette.break_b;
  long_break_color := rgb palette.lb_r palette.lb_g palette.lb_b

let draw_palette y palette =
  set_color (rgb palette.work_r palette.work_g palette.work_b);
  fill_rect palette_display_x y (windowsizex / 9 * 2) ((windowsizey / 8) - 20);
  set_color (rgb palette.break_r palette.break_g palette.break_b);
  fill_rect
    (palette_display_x + (windowsizex / 9 * 2))
    y
    (windowsizex / 9 * 2)
    ((windowsizey / 8) - 20);
  set_color (rgb palette.lb_r palette.lb_g palette.lb_b);
  fill_rect
    (palette_display_x + (2 * (windowsizex / 9 * 2)))
    y
    (windowsizex / 9 * 2)
    ((windowsizey / 8) - 20);
  set_color (rgb 0 0 0)

let change_time_to_input t =
  let hours = get_number_input () in
  let mins = get_number_input () in
  let secs = get_number_input () in
  t := seconds_from_triple (hours, mins, secs)

let clear_palette_selection_display () =
  set_color (rgb 255 255 255);
  draw_rect (palette_display_x - 2)
    ((windowsizey / 4 * 3) - 2)
    palette_display_width
    (palette_display_height + 4);
  draw_rect (palette_display_x - 2)
    ((windowsizey / 8 * 5) - 2)
    palette_display_width
    (palette_display_height + 4);
  draw_rect (palette_display_x - 2)
    ((windowsizey / 2) - 2)
    palette_display_width
    (palette_display_height + 4);
  draw_rect (palette_display_x - 2)
    ((windowsizey / 8 * 3) - 2)
    palette_display_width
    (palette_display_height + 4);
  draw_rect (palette_display_x - 2)
    ((windowsizey / 4) - 2)
    palette_display_width
    (palette_display_height + 4);
  set_color (rgb 0 0 0)

let change_color_menu () =
  moveto (windowsizex / 3) 0;
  draw_string "Press [esc] to exit.";
  draw_palette (windowsizey / 4 * 3) palette1;
  draw_palette (windowsizey / 8 * 5) palette2;
  draw_palette (windowsizey / 2) palette3;
  draw_palette (windowsizey / 8 * 3) palette4;
  draw_palette (windowsizey / 4) palette5;
  draw_rect (palette_display_x - 2)
    (!current_color_scheme_y_pos - 2)
    palette_display_width
    (palette_display_height + 4);
  set_timer_colors palette1;

  let select_color_palette (y : int) (palette : palette) : unit =
    clear_palette_selection_display ();
    current_color_scheme_y_pos := y;
    draw_rect (palette_display_x - 2) (y - 2) palette_display_width
      (palette_display_height + 4);
    set_timer_colors palette
  in
  let close_color_menu = ref false in
  while !close_color_menu = false do
    let current_status = wait_next_event [ Button_down; Key_pressed ] in
    if
      current_status.keypressed = false
      && in_rectangle current_status.mouse_x current_status.mouse_y
           palette_display_x
           (windowsizey / 4 * 3)
           palette_display_width palette_display_height
    then select_color_palette (windowsizey / 4 * 3) palette1
    else if
      current_status.keypressed = false
      && in_rectangle current_status.mouse_x current_status.mouse_y
           palette_display_x
           (windowsizey / 8 * 5)
           palette_display_width palette_display_height
    then select_color_palette (windowsizey / 8 * 5) palette2
    else if
      current_status.keypressed = false
      && in_rectangle current_status.mouse_x current_status.mouse_y
           palette_display_x (windowsizey / 2) palette_display_width
           palette_display_height
    then select_color_palette (windowsizey / 2) palette3
    else if
      current_status.keypressed = false
      && in_rectangle current_status.mouse_x current_status.mouse_y
           palette_display_x
           (windowsizey / 8 * 3)
           palette_display_width palette_display_height
    then select_color_palette (windowsizey / 8 * 3) palette4
    else if
      current_status.keypressed = false
      && in_rectangle current_status.mouse_x current_status.mouse_y
           palette_display_x (windowsizey / 4) palette_display_width
           palette_display_height
    then select_color_palette (windowsizey / 4) palette5
    else if current_status.keypressed = true && current_status.key = '\027' then
      close_color_menu := true
    else ()
  done

let settings_menu () =
  let close_settings = ref false in
  while !close_settings = false do
    clear_graph ();
    moveto (windowsizex / 3) (windowsizey - 20);

    draw_string "Configure Settings";
    moveto (windowsizex / 3) 0;
    draw_string "Press [esc] to exit.";
    moveto (windowsizex / 10) (windowsizey / 3 * 2);
    draw_string
      ("Press 'w' to adjust the duration of a work session. Current duration: "
     ^ time_string work_time);
    moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 20);
    draw_string
      ("Press 'b' to adjust the duration of a break session. Current duration: "
     ^ time_string break_time);
    moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 40);
    draw_string
      ("Press 'l' to adjust the duration of a long break session. Current \
        duration: "
      ^ time_string long_break_time);
    moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 80);
    draw_string
      ("Press 'n' to adjust the number of work sessions before a long break. \
        Current number: " ^ string_of_int !cycles);
    moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 120);
    draw_string "Press 'c' to adjust the timer's colors.";
    let current_status = wait_next_event [ Key_pressed ] in
    if current_status.key = '\027' then (
      clear_graph ();
      close_settings := true)
    else if current_status.key = 'w' then (
      clear_graph ();
      moveto (windowsizex / 10) (windowsizey / 3 * 2);
      draw_string "Now changing WORK duration";
      change_time_menu work_time)
    else if current_status.key = 'b' then (
      clear_graph ();
      moveto (windowsizex / 10) (windowsizey / 3 * 2);
      draw_string "Now changing SHORT BREAK duration";
      change_time_menu break_time)
    else if current_status.key = 'l' then (
      clear_graph ();
      moveto (windowsizex / 10) (windowsizey / 3 * 2);
      draw_string "Now changing LONG BREAK duration";
      change_time_menu long_break_time)
    else if current_status.key = 'n' then (
      clear_graph ();
      moveto (windowsizex / 10) (windowsizey / 3 * 2);
      draw_string "Now changing cycle length";
      moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 20);
      draw_string
        ("Currently requiring " ^ string_of_int !cycles
       ^ " work sessions before a long break.");
      moveto (windowsizex / 10) ((windowsizey / 3 * 2) - 40);
      draw_string "Enter a new value and press [enter].";
      cycles := int_of_string (get_number_input ()))
    else if current_status.key = 'c' then (
      clear_graph ();
      moveto (windowsizex / 3) (windowsizey - 20);
      draw_string "Select a color scheme";
      change_color_menu ())
    else ()
  done

let rec run_cycle cat_frame_index buffer_catdisplay catframes direction catposx
    catposy start_time catspritesizex catspritesizey total_seconds
    grassimgstodraw grasspositions grassarraycapacity grassbuffer
    superimposeitems flowerbed =
  clear_graph ();
  let description t =
    match t.status with
    | Work -> "Working for " ^ time_string work_time
    | Break -> "On break for " ^ time_string break_time
    | LongBreak -> "On break for " ^ time_string long_break_time
  in
  moveto (windowsizex / 4) (windowsizey / 8 * 7);
  set_color (rgb 0 0 0);
  draw_string (description time_obj);

  let set_prog_color t =
    match t.status with
    | Work -> !work_color
    | Break -> !break_color
    | LongBreak -> !long_break_color
  in

  let prog_color = set_prog_color time_obj in

  let progress = (Unix.time () -. start_time) /. float_of_int total_seconds in

  set_color prog_color;

  let endangle = int_of_float (90. -. (progress *. 360.)) in
  fill_arc timerarcx timerarcy timerarcrx timerarcry timerstartangle endangle;

  let grassimgstodraw', grasspositions', grassarraycapacity', grassbuffer' =
    if grassarraycapacity = 17 * 25 then
      (grassimgstodraw, grasspositions, grassarraycapacity, grassbuffer)
    else if grassbuffer = 2000 then (
      let grassimgstodraw', grasspositions' =
        proc_generate Grass.names Grass.images grassimgstodraw grasspositions
          superimposeitems grassarraycapacity
      in
      (if grassarraycapacity mod 106 = 0 && !catbuffertime <> 0 then
       let proportionfilled = 17 * 25 / (grassarraycapacity + 1) in
       let bufferchange =
         match proportionfilled with
         | 0 -> 0
         | 1 -> 50
         | 2 -> 500
         | 3 -> 400
         | _ -> 0
       in
       catbuffertime := !catbuffertime - bufferchange);
      (grassimgstodraw', grasspositions', grassarraycapacity + 1, 0))
    else (grassimgstodraw, grasspositions, grassarraycapacity, grassbuffer + 1)
  in
  draw_items grassimgstodraw' grassarraycapacity';

  let ( new_frame_index,
        new_buffer_index,
        new_catframes,
        new_direction,
        new_posx,
        new_posy ) =
    if buffer_catdisplay >= !catbuffertime then
      if cat_frame_index = Array.length catframes - 1 then
        let newdirection =
          if !changedirectionbuffer = 200 then (
            changedirectionbuffer := 0;
            changedirectionthreshold := Random.int 500;
            Random.int 4)
          else (
            changedirectionbuffer := !changedirectionbuffer + 50;
            direction)
        in
        let newanimations =
          CatWalk.animate newdirection catposx catposy catspritesizex
            catspritesizey
        in
        ( 0,
          0,
          snd newanimations,
          (fst newanimations).(0),
          (fst newanimations).(1),
          (fst newanimations).(2) )
      else (cat_frame_index + 1, 0, catframes, direction, catposx, catposy)
    else
      ( cat_frame_index,
        buffer_catdisplay + 1,
        catframes,
        direction,
        catposx,
        catposy )
  in

  draw_items [| catframes.(cat_frame_index) |] 1;

  let arr_fb = Array.of_list flowerbed in
  draw_items arr_fb (Array.length arr_fb);
  synchronize ();

  if Unix.time () -. start_time < float_of_int total_seconds then
    run_cycle new_frame_index new_buffer_index new_catframes new_direction
      new_posx new_posy start_time catspritesizex catspritesizey total_seconds
      grassimgstodraw' grasspositions' grassarraycapacity' grassbuffer'
      superimposeitems flowerbed
  else
    ( new_frame_index,
      new_buffer_index,
      new_catframes,
      new_direction,
      new_posx,
      new_posy,
      grassimgstodraw',
      grasspositions',
      grassarraycapacity',
      grassbuffer' )

let rec run_gui grassimgs grasspositions grassarraycapacity grassbuffer
    superimposeitems direction catposx catposy cat_frame_index buffer_catdisplay
    catframes catspritesizex catspritesizey flowerbed =
  let total_seconds = time_obj.time in
  open_graph "";
  resize_window windowsizex windowsizey;
  set_window_title "Productivity Timer";
  set_color (rgb 0 0 0);
  auto_synchronize false;

  let clicked = ref false in
  while !clicked = false do
    moveto (int_of_float (float_of_int (size_x ()) /. 2.45)) (size_y () - 20);

    draw_string "Press 's' to open settings.";
    moveto (int_of_float (float_of_int (size_x ()) /. 2.52)) (size_y () / 2);

    draw_string (select_string time_obj);
    let current_status = wait_next_event [ Button_down; Key_pressed ] in
    if current_status.button then clicked := true
    else if current_status.key = 's' then settings_menu ()
    else ()
  done;

  let start_time = Unix.time () in

  let flowerbed' =
    match time_obj.status with
    | Break | LongBreak ->
        let op = { default_options with colored = true } in
        let new_tulip = px_array 16 32 [||] true true op in
        let posx, posy =
          (Random.int (windowsizey - gap), Random.int (windowsizex - gap))
        in
        ("reward", (make_image new_tulip, (posx, posy))) :: flowerbed
    | Work -> [ ("work", (make_image [| [| 1 |] |], (0, 0))) ]
  in
  let ( new_frame_index,
        new_buffer_index,
        new_catframes,
        new_direction,
        new_posx,
        new_posy,
        grassimgs',
        grasspositions',
        grassarraycapacity',
        grassbuffer' ) =
    run_cycle cat_frame_index buffer_catdisplay catframes direction catposx
      catposy start_time catspritesizex catspritesizey !total_seconds grassimgs
      grasspositions grassarraycapacity grassbuffer superimposeitems flowerbed'
  in
  let circlex = int_of_float (float_of_int windowsizex *. 0.85) in
  let circley = int_of_float (float_of_int windowsizey *. 0.85) in
  let circler = windowsizex / 10 in
  fill_circle circlex circley circler;

  change_status time_obj;

  run_gui grassimgs' grasspositions' grassarraycapacity' grassbuffer'
    superimposeitems new_direction new_posx new_posy new_frame_index
    new_buffer_index new_catframes catspritesizex catspritesizey flowerbed'

let start () =
  let catdirection = Random.int 4 in
  let catstartingposx = 400 in
  let catstartingposy = 400 in
  let catstartframes =
    CatWalk.animate catdirection catstartingposx catstartingposy
      CatWalk.imagesizex CatWalk.imagesizey
  in
  let endposx = (fst catstartframes).(1) in
  let endposy = (fst catstartframes).(2) in
  run_gui
    (Array.make 2500 ("", (Graphics.make_image [| [| 1 |] |], (0, 0))))
    (Hashtbl.create 2500) 0 0 false catdirection endposx endposy 0 0
    (snd catstartframes) CatWalk.imagesizex CatWalk.imagesizey [ flowerbed ]
