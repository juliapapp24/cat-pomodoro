open Graphics
open Img_gen
open CONSTANTS
open Items.AssetDisplay
open Grass

let rec genpos tilesize imgpositions =
  let seedx = tilesize * Random.int (windowsizex / tilesize) in
  let seedy = tilesize * Random.int (windowsizex / tilesize) in
  if
    seedy > windowsizey - topbuffertimer - tilesize
    || seedy < bottombuffertimer
    ||
    match Hashtbl.find_opt imgpositions (seedx, seedy) with
    | Some v -> true
    | None -> false
  then genpos tilesize imgpositions
  else (seedx, seedy)

let proc_generate names itemimgs itemstodraw imgpositions superimpose arraysize
    =
  let itemname = names.(Random.int (Array.length names)) in
  let itemsizex, itemsizey = (Grass.imagesizex, Grass.imagesizey) in
  let itemimg = Hashtbl.find itemimgs itemname in
  let posx, posy =
    if superimpose then
      ( Random.int (windowsizex - itemsizex),
        Random.int (windowsizey - itemsizey) )
    else genpos Grass.imagesizex imgpositions
  in
  itemstodraw.(arraysize) <- (itemname, (itemimg, (posx, posy)));
  Hashtbl.add imgpositions (posx, posy) itemname;
  (itemstodraw, imgpositions)

let draw_items itemstodraw arraysize =
  for i = 0 to arraysize - 1 do
    let item = snd itemstodraw.(i) in
    let itemimg = fst item in
    let posx = fst (snd item) in
    let posy = snd (snd item) in

    Graphics.draw_image itemimg posx posy
  done
