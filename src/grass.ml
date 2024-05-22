open Items.AssetDisplay
open CONSTANTS

module Grass = struct
  let tilesizex = 16
  let tilesizey = 16
  let imagesizex = 32
  let imagesizey = 32
  let speed = 0
  let numrows = 12
  let numcols = 10
  let filename = dataprefix ^ "grass" ^ Filename.dir_sep ^ "grass.png"

  let names =
    tilenames
      (Yojson.Basic.from_file
         (dataprefix ^ "grass" ^ Filename.dir_sep ^ "grass.json"))

  let images =
    buildimgs filename names numrows numcols tilesizex tilesizey ratioconstant
end