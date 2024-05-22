let windowsizex = 800
let windowsizey = 800
let gap = 32
let dataprefix = "data" ^ Filename.dir_sep
let timerarcx = int_of_float (float_of_int windowsizex *. 0.85)
let timerarcy = int_of_float (float_of_int windowsizey *. 0.85)
let timerarcrx = windowsizex / 10
let timerarcry = windowsizey / 10
let timerstartangle = 90
let topbuffertimer = windowsizey - (timerarcy - timerarcry)
let bottombuffertimer = windowsizey / 25
let default_work_time = ref (25 * 60)
let default_break_time = ref (5 * 60)
let default_long_break_time = ref (15 * 60)
let default_cycles = 4
let palette_display_height = (windowsizey / 8) - 20
let palette_display_width = windowsizex / 3 * 2
let palette_display_x = windowsizex / 6
let current_color_scheme_y_pos = ref (windowsizey / 4 * 3)
let ratioconstant = 2
let catbuffertime = ref 850
let changedirectionbuffer = ref 0
let changedirectionthreshold = ref 200


type palette = {
  work_r : int;
  work_g : int;
  work_b : int;
  break_r : int;
  break_g : int;
  break_b : int;
  lb_r : int;
  lb_g : int;
  lb_b : int;
}

let palette1 : palette =
  (*via https://colorkit.co/palette/ff595e-ffca3a-8ac926-1982c4-6a4c93/ *)
  {
    work_r = 25;
    work_g = 130;
    work_b = 196;
    break_r = 255;
    break_g = 89;
    break_b = 94;
    lb_r = 138;
    lb_g = 201;
    lb_b = 38;
  }

let palette2 : palette =
  (*via https://coolors.co/palette/156064-00c49a-f8e16c *)
  {
    work_r = 21;
    work_g = 96;
    work_b = 100;
    break_r = 0;
    break_g = 196;
    break_b = 154;
    lb_r = 248;
    lb_g = 225;
    lb_b = 108;
  }

let palette3 : palette =
  (*via https://colorkit.co/palette/a0b0e2-cdb4e6-fab9ea/ *)
  {
    work_r = 160;
    work_g = 176;
    work_b = 226;
    break_r = 205;
    break_g = 180;
    break_b = 230;
    lb_r = 250;
    lb_g = 185;
    lb_b = 234;
  }

let palette4 : palette =
  (*via https://colorhunt.co/palette/52006acd113bff7600ffa900 *)
  {
    work_r = 82;
    work_g = 0;
    work_b = 106;
    break_r = 205;
    break_g = 17;
    break_b = 59;
    lb_r = 255;
    lb_g = 118;
    lb_b = 0;
  }

let palette5 : palette =
  (*via https://colorhunt.co/palette/06283d256d8547b5ffdff6ff *)
  {
    work_r = 6;
    work_g = 40;
    work_b = 61;
    break_r = 37;
    break_g = 109;
    break_b = 133;
    lb_r = 71;
    lb_g = 181;
    lb_b = 255;
  }
