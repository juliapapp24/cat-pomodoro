open Items.AssetDisplay
open Displayillustration
open CONSTANTS

let catprefix = dataprefix ^ "cat" ^ Filename.dir_sep

let catframe frames positionsx positionsy index =
  let framename = fst frames.(index) in
  let frameimg = snd frames.(index) in
  let posx = positionsx.(index) in
  let posy = positionsy.(index) in
  (framename, (frameimg, (posx, posy)))

module CatIdle = struct
  let namedata = Yojson.Basic.from_file (catprefix ^ "idle.json")
  let filename = catprefix ^ "idle.png"
  let speed = 0
  let numrows = 2
  let numcols = 4
  let tilesizex = 16
  let tilesizey = 17
  let imagesizex = 32
  let imagesizey = 34
  let names = tilenames namedata

  let images =
    let imgs =
      buildimgs filename names numrows (numcols - 1) tilesizex tilesizey
        ratioconstant
    in

    Hashtbl.add imgs "leftup" (flip (Hashtbl.find imgs "rightup"));
    Hashtbl.add imgs "leftdown" (flip (Hashtbl.find imgs "rightdown"));
    imgs
end

module CatPunch = struct
  let filename = catprefix ^ "punch.png"
  let speed = 0
  let numrows = 4
  let numcols = 3
  let tilesizex = 16
  let tilesizey = 17
  let imagesizex = 32
  let imagesizey = 34
  let names = tilenames (Yojson.Basic.from_file (catprefix ^ "punch.json"))

  let images =
    buildimgs filename names numrows numcols tilesizex tilesizey ratioconstant
end

module CatWalk = struct
  exception InvalidDirection

  let filename = dataprefix ^ "cat" ^ Filename.dir_sep ^ "walk.png"
  let speed = 5
  let numrows = 4
  let numcols = 4
  let tilesizex = 16
  let tilesizey = 16
  let imagesizex = 32
  let imagesizey = 32
  let names = tilenames (Yojson.Basic.from_file (catprefix ^ "walk.json"))

  let images =
    let imgs =
      buildimgs filename names numrows (numcols - 1) tilesizex tilesizey
        ratioconstant
    in
    Hashtbl.add imgs "leftstill" (flip (Hashtbl.find imgs "rightstill"));
    Hashtbl.add imgs "leftrightwalk" (flip (Hashtbl.find imgs "rightrightwalk"));
    Hashtbl.add imgs "leftleftwalk" (flip (Hashtbl.find imgs "rightleftwalk"));
    imgs

  let newcatpos direction oldx oldy =
    match direction with
    | 0 -> (oldx + speed, oldy)
    | 1 -> (oldx, oldy + speed)
    | 2 -> (oldx - speed, oldy)
    | 3 -> (oldx, oldy - speed)
    | _ -> raise InvalidDirection

  let collisiondetection direction posx posy spritesizex spritesizey =
    let spacetoright = windowsizex - posx - (2 * spritesizex) in
    let spacetoleft = posx - (2 * spritesizex) in
    let spacetotop = windowsizey - posy - (2 * spritesizey) - topbuffertimer in
    let spacetobottom = posy - (2 * spritesizey) - bottombuffertimer in
    match direction with
    | 0 -> if spacetoright <= 0 then true else false
    | 1 -> if spacetobottom <= 0 then true else false
    | 2 -> if spacetoleft <= 0 then true else false
    | 3 -> if spacetotop <= 0 then true else false
    | _ -> raise InvalidDirection

  let rec redirectcollision olddirection posx posy spritesizex spritesizey =
    let newdirection = Random.int 4 in
    if
      newdirection = olddirection
      || collisiondetection newdirection posx posy spritesizex spritesizey
    then redirectcollision olddirection posx posy spritesizex spritesizey
    else newdirection

  let animate direction posx posy spritesizex spritesizey =
    let collision =
      collisiondetection direction posx posy spritesizex spritesizey
    in
    let ( newdirection,
          speed,
          spritesizex,
          spritesizey,
          positionsleft,
          positionsright,
          positionsup,
          positionsdown,
          nrows ) =
      match collision with
      | true ->
          let newspeed, spritesizex, spritesizey, nrows =
            ( CatIdle.speed,
              CatIdle.imagesizex,
              CatIdle.imagesizey,
              CatIdle.numrows )
          in
          ( redirectcollision direction posx posy spritesizex spritesizey,
            newspeed,
            spritesizex,
            spritesizey,
            [|
              ("leftup", Hashtbl.find CatIdle.images "leftup");
              ("leftdown", Hashtbl.find CatIdle.images "leftdown");
            |],
            [|
              ("rightup", Hashtbl.find CatIdle.images "rightup");
              ("rightdown", Hashtbl.find CatIdle.images "rightdown");
            |],
            [|
              ("backwardup", Hashtbl.find CatIdle.images "backwardup");
              ("backwarddown", Hashtbl.find CatIdle.images "backwarddown");
            |],
            [|
              ("forwardup", Hashtbl.find CatIdle.images "forwardup");
              ("forwarddown", Hashtbl.find CatIdle.images "forwarddown");
            |],
            nrows )
      | false ->
          ( direction,
            speed,
            spritesizex,
            spritesizey,
            [|
              ("leftstill", Hashtbl.find images "leftstill");
              ("leftrightwalk", Hashtbl.find images "leftrightwalk");
              ("leftstill", Hashtbl.find images "leftstill");
              ("leftleftwalk", Hashtbl.find images "leftleftwalk");
            |],
            [|
              ("rightstill", Hashtbl.find images "rightstill");
              ("rightrightwalk", Hashtbl.find images "rightrightwalk");
              ("rightstill", Hashtbl.find images "rightstill");
              ("rightleftwalk", Hashtbl.find images "rightleftwalk");
            |],
            [|
              ("backwardstill", Hashtbl.find images "backwardstill");
              ("backwardrightwalk", Hashtbl.find images "backwardrightwalk");
              ("backwardstill", Hashtbl.find images "backwardstill");
              ("backwardleftwalk", Hashtbl.find images "backwardleftwalk");
            |],
            [|
              ("forwardstill", Hashtbl.find images "forwardstill");
              ("forwardrightwalk", Hashtbl.find images "forwardrightwalk");
              ("forwardstill", Hashtbl.find images "forwardstill");
              ("forwardleftwalk", Hashtbl.find images "forwardleftwalk");
            |],
            numrows )
    in

    let imagestodraw =
      match newdirection with
      | 0 -> positionsright
      | 1 -> positionsdown
      | 2 -> positionsleft
      | 3 -> positionsup
      | _ -> raise InvalidDirection
    in
    let xpositions, ypositions =
      match newdirection with
      | 0 ->
          ( Array.init nrows (fun x -> posx + (speed * x)),
            Array.init nrows (fun x -> posy) )
      | 1 ->
          ( Array.init nrows (fun x -> posx),
            Array.init nrows (fun x -> posy - (speed * x)) )
      | 2 ->
          ( Array.init nrows (fun x -> posx - (speed * x)),
            Array.init nrows (fun x -> posy) )
      | 3 ->
          ( Array.init nrows (fun x -> posx),
            Array.init nrows (fun x -> posy + (speed * x)) )
      | _ -> raise InvalidDirection
    in
    let newposx = xpositions.(nrows - 1) in
    let newposy = ypositions.(nrows - 1) in

    ( [| newdirection; newposx; newposy |],
      Array.init
        (Array.length imagestodraw)
        (catframe imagestodraw xpositions ypositions) )
end