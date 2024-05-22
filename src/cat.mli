open Asset

val catprefix : string
(** *)

val catframe :
  (string * Graphics.image) array ->
  int array ->
  int array ->
  int ->
  string * (Graphics.image * (int * int))
(** *)

module CatWalk : Walk
