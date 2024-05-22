val genpos : int -> (int * int, string) Hashtbl.t -> int * int
(** *)
val proc_generate :
  string array ->
  (string, Graphics.image) Hashtbl.t ->
  (string * (Graphics.image * (int * int))) array ->
  (int * int, string) Hashtbl.t ->
  bool ->
  int ->
  (string * (Graphics.image * (int * int))) array
  * (int * int, string) Hashtbl.t
(** *)

val draw_items: (string * (Graphics.image * (int * int))) array -> int -> unit
(** *)