open Graphics

module type Asset = sig
  val filename : string
  val speed : int
  val numrows : int
  val numcols : int
  val tilesizex : int
  val tilesizey : int
  val imagesizex : int
  val imagesizey : int
  val names : string array
  val images : (string, Graphics.image) Hashtbl.t
end

(** *)
module type Display = sig 
  val rotate: Graphics.color array array -> Graphics.color array array -> float -> unit
  (** *)
  val crop: Graphics.color array array -> int -> int -> int -> int -> Graphics.color array array
  (** *)
  val make_transparent: Graphics.color array array -> unit
  (** *)
  val expandwidth: Graphics.color array -> int -> Graphics.color array
  (** *)
  val enlarge: Graphics.color array array -> int -> Graphics.color array array 
  (** *)
  val flip: Graphics.image -> Graphics.image 
  (** *)
  val insertimg: Graphics.image -> (string, Graphics.image) Hashtbl.t -> string -> int -> int * int -> int * int -> (string, Graphics.image) Hashtbl.t
  (** *)
  val makestartcoords: int -> int -> int list 
  (** *)
  val makexstarts: int -> int -> int list 
  (** *)
  val makeystarts: int -> int -> int list 
  (** *)
  val tilenames: Yojson.Basic.t -> string array 
  (** *)
  val createassetinfo: 'a array -> int -> int -> int -> int list -> int list -> int -> 'a * ((int * int) * (int * int))
  (** *)
  val assetinfo: string array -> int -> int -> int -> int -> (string * ((int * int) * (int * int))) list
  (** *)
  val buildimgs: string -> string array -> int -> int -> int -> int -> int -> (string, Graphics.image) Hashtbl.t 
  (** *)
end

(** *)
module type Walk = sig
  exception InvalidDirection
  (** *)

  val newcatpos : int -> int -> int -> int * int
  (** *)

  val collisiondetection : int -> int -> int -> int -> int -> bool
  (** *)

  val redirectcollision : int -> int -> int -> int -> int -> int
  (** *)

  val animate :
    int ->
    int ->
    int ->
    int ->
    int ->
    int array * (string * (Graphics.image * (int * int))) array
  (** *)

  include Asset
end