open Images
open Graphics
open CONSTANTS
open Yojson.Basic.Util

module AssetDisplay = struct
  (**Inspired by https://stackoverflow.com/questions/50108152/load-image-from-file-in-ocaml-graphics*)
  let rotate src dst angle =
    let sh, sw = Array.(float (length src), float (length src.(0))) in
    let dh, dw = Array.(length dst, length dst.(0)) in
    let dx, dy = (float dw /. 2., float dh /. 2.) in
    let scale = min (float dh /. sh) (float dw /. sw) in
    let duCol = sin (-.angle) *. (1. /. scale) in
    let dvCol = cos (-.angle) *. (1. /. scale) in
    let rec col dst x u v =
      if x < dw then (
        dst.(x) <-
          (if (0. <= u && u < sw) && 0. <= v && v < sh then
           src.(truncate v).(truncate u)
          else Graphics.white);
        col dst (x + 1) (u +. dvCol) (v -. duCol))
    in
    let rec row y rowu rowv =
      if y < dh then (
        col dst.(y) 0 rowu rowv;
        row (y + 1) (rowu +. duCol) (rowv +. dvCol))
    in
    row 0
      ((sw /. 2.) -. ((dx *. dvCol) +. (dy *. duCol)))
      ((sh /. 2.) -. ((dx *. -.duCol) +. (dy *. dvCol)))

  let crop colmatrix xstart xend ystart yend =
    let rowindexstart = ystart in
    let rowindexend = yend in
    let nrows = rowindexend - rowindexstart + 1 in
    let colindexstart = xstart in
    let colindexend = xend in
    let ncols = colindexend - colindexstart + 1 in
    let newcolmatrix = Array.init nrows (fun _ -> [||]) in
    for x = 0 to Array.length newcolmatrix - 1 do
      newcolmatrix.(x) <-
        Array.sub colmatrix.(rowindexstart + x) colindexstart ncols
    done;
    newcolmatrix

  let make_transparent colmatrix =
    for i = 0 to Array.length colmatrix - 1 do
      for j = 0 to Array.length colmatrix.(i) - 1 do
        if colmatrix.(i).(j) = rgb 255 255 255 then colmatrix.(i).(j) <- transp
      done
    done

  let expandwidth colmatrixrow ratio =
    let newcolmatrixrow =
      Array.make (Array.length colmatrixrow * ratio) colmatrixrow.(0)
    in
    for j = 0 to Array.length colmatrixrow - 1 do
      for k = 0 to ratio - 1 do
        newcolmatrixrow.((j * ratio) + k) <- colmatrixrow.(j)
      done
    done;
    newcolmatrixrow

  let enlarge colmatrix ratio =
    let newcolmatrix = Array.make (ratio * Array.length colmatrix) [||] in
    for x = 0 to Array.length colmatrix - 1 do
      for y = 0 to ratio - 1 do
        newcolmatrix.((x * ratio) + y) <- expandwidth colmatrix.(x) ratio
      done
    done;
    newcolmatrix

  let flip image =
    let flip_row r =
      let rlength = Array.length r - 1 in
      let newr = Array.init (rlength + 1) (fun i -> r.(rlength - i)) in
      newr
    in
    let image = Graphics.dump_image image in
    for x = 0 to Array.length image - 1 do
      image.(x) <- flip_row image.(x)
    done;
    Graphics.make_image image

  let insertimg assetgraphic itemimgs name ratio (xstart, xend) (ystart, yend) =
    let itemimg =
      enlarge
        (crop (Graphics.dump_image assetgraphic) xstart xend ystart yend)
        ratio
    in
    make_transparent itemimg;
    if name = "darkgreen7" then
      rotate
        (enlarge
           (crop (Graphics.dump_image assetgraphic) xstart xend ystart yend)
           ratio)
        itemimg 0.;
    Hashtbl.add itemimgs name (Graphics.make_image itemimg);
    itemimgs

  let makestartcoords numtiles tsize = List.init numtiles (fun x -> x * tsize)
  let makexstarts ncol tsizex = makestartcoords ncol tsizex
  let makeystarts nrow tsizey = makestartcoords nrow tsizey

  let tilenames json =
    json |> member "tiles" |> to_list |> List.map to_string |> Array.of_list

  let createassetinfo names ncol tsizex tsizey xstarts ystarts index =
    let tilename = names.(index) in
    let tilexstart = List.nth xstarts (index mod ncol) in
    let tileystart = List.nth ystarts ((index - (index mod ncol)) / ncol) in
    ( tilename,
      ( (tilexstart, tilexstart + tsizex - 1),
        (tileystart, tileystart + tsizey - 1) ) )

  let assetinfo names nrow ncol tsizex tsizey =
    let xstarts = makexstarts ncol tsizex in
    let ystarts = makeystarts nrow tsizey in
    List.init (nrow * ncol)
      (createassetinfo names ncol tsizex tsizey xstarts ystarts)

  let buildimgs assetpk names nrow ncol tsizex tsizey ratio =
    let assetimg = Png.load assetpk [] in
    let assetgraphic =
      let _ = Graphics.open_graph "" in
      Graphic_image.of_image assetimg
    in

    List.fold_left
      (fun itemimgsacc item ->
        insertimg assetgraphic itemimgsacc (fst item) ratio
          (fst (snd item))
          (snd (snd item)))
      (Hashtbl.create (nrow * ncol))
      (assetinfo names nrow ncol tsizex tsizey)
end
