module tetris

open Canvas

type Color =
    | Yellow
    | Cyan
    | Blue
    | Orange
    | Red
    | Green
    | Purple

type Action =
    |MoveRight
    |MoveLeft
    |MoveDown
    |RotateRight

let fromColor (c: Color) : Canvas.color =
    match c with
        | Yellow -> fromRgb(255, 255, 0)
        | Cyan -> fromRgb(0, 255, 255)
        | Blue -> fromRgb(0, 0, 255)
        | Orange -> fromRgb(255,127,0)
        | Red -> fromRgb(255,0,0)
        | Green -> fromRgb(0, 255, 0)
        | Purple -> fromRgb(128, 0, 128)

type position = int*int
type tetromino (a: bool[,], c:Color, o:position) =
    member val image = a with get, set
    member this.col = c
    member val offset = o with get, set
    member this.width() = this.image|> Array2D.length2
    member this.height() = this.image|> Array2D.length1
    override this.ToString () =
        let replacestring (oldV:string) (newV:string) (image:string) =
            image.Replace(oldV, newV)
        let piecestring = sprintf"%A" this.image
        piecestring
        |> replacestring "[" ""
        |> replacestring "]" ""
        |> replacestring "false" "_"
        |> replacestring "true" "#"
    member this.moveRight() =
        let x,y = this.offset
        ((x+1,y), this.image)
    member this.moveLeft() =
        let x,y = this.offset
        ((x-1,y),this.image)
    member this.moveDown() =
        let x,y = this.offset
        ((x,y+1),this.image)
    member this.rotateRight() : ((int*int)*bool[,]) =
        let rotated = Array2D.create (this.width()) (this.height()) false
        this.image |> Array2D.iteri (fun i j v ->
            if (i - (this.width()-1)) >= 0 then
                rotated[j,(i-(this.width()-1))] <- v
            else
                rotated[j,(-(i-(this.width()-1)))] <- v
        )
        (this.offset, rotated)
    member this.setPiece (pos: position) (img: bool[,]) =
        this.image <- img
        this.offset <- pos
        ()


let squareimage =
    Array2D.create 2 2 true

let straightimage =
    let arr = Array2D.create 4 4 false
    arr[1,0] <- true
    arr[1,1] <- true
    arr[1,2] <- true
    arr[1,3] <- true
    arr

let timage =
    let arr = Array2D.create 3 3 true
    arr[0,0] <- false
    arr[0,2] <- false
    arr[2,0] <- false
    arr[2,1] <- false
    arr[2,2] <- false
    arr

let limage =
    let arr = Array2D.create 3 3 true
    arr[0,0] <- false
    arr[0,1] <- false
    arr[2,0] <- false
    arr[2,1] <- false
    arr[2,2] <- false
    arr

let lmirrorimage =
    let arr = Array2D.create 3 3 true
    arr[0,2] <- false
    arr[0,1] <- false
    arr[2,0] <- false
    arr[2,1] <- false
    arr[2,2] <- false
    arr

let zimage =
    let arr = Array2D.create 3 3 true
    arr[0,0] <- false
    arr[1,2] <- false
    arr[2,0] <- false
    arr[2,1] <- false
    arr[2,2] <- false
    arr

let zmirrorimage =
    let arr = Array2D.create 3 3 true
    arr[1,0] <- false
    arr[0,2] <- false
    arr[2,0] <- false
    arr[2,1] <- false
    arr[2,2] <- false
    arr

type square (o:position) =
    inherit tetromino (squareimage, Yellow, o)

type straight (o:position) =
    inherit tetromino (straightimage, Cyan, o)

type t (o:position) =
    inherit tetromino (timage, Purple,o)

type l (o:position, m:bool) =
    inherit tetromino ((if m then lmirrorimage else limage),(if m then Blue else Orange), o)

type z (o:position, m:bool) =
    inherit tetromino ((if m then zmirrorimage else zimage),(if m then Red else Green), o)


type board (w: int , h: int) =
    let _board = Array2D.create w h None
    let mutable activePiece : tetromino option = None
    let mutable _score : int = 0
    override this.ToString () =
        let replacestring (oldV:string) (newV:string) (image:string) =
            image.Replace(oldV, newV)
        let acc = Array2D.create h w None
        for i = 0 to (this.board|> Array2D.length1) - 1 do
            for j = 0 to (this.board|> Array2D.length2) - 1 do
                acc[j,i] <- this.board[i,j]
        let boardstring = sprintf"%A" acc
        boardstring
        |> replacestring "[\n" ""
        |> replacestring "]" ""
        |> replacestring "None" " "
        |> replacestring "Some Yellow" "Y"
        |> replacestring "Some Cyan" "C"
        |> replacestring "Some Blue" "B"
        |> replacestring "Some Orange" "O"
        |> replacestring "Some Red" "R"
        |> replacestring "Some Green" "G"
        |> replacestring "Some Purple" "P"

    member this.newPiece() : tetromino option =
        let rnd = System.Random()
        let rnd1 = rnd.Next(7)
        let newPiece =
            match rnd1 with
            |0 -> new square(4,0) :> tetromino |> Some
            |1 -> new straight(3,-1) :> tetromino |> Some
            |2 -> new l((3,0), false) :> tetromino |> Some
            |3 -> new l((3,0), true) :> tetromino |> Some
            |4 -> new t(3,0) :> tetromino |> Some
            |5 -> new z((3,0), false) :> tetromino |> Some
            |6 -> new z((3,0), true) :> tetromino |> Some
            |_ -> None
        let piece = newPiece |> Option.get
        if not (this.checkCollisionPiece piece.offset piece.image) then
            newPiece
        else
            None

    member this.put (t: tetromino) : unit =
        let pieceToPlace = (Array2D.create (t.height()) (t.width())(None))
        for i = 0 to (t.width()) - 1 do
            for j = 0 to (t.height() - 1) do
                if this.board.[i,j] = None then
                    if t.image.[j,i] then
                        pieceToPlace[j,i] <- Some t.col
                        ()
                    else
                        pieceToPlace[j,i] <- None
                        ()
                else
                    ()
        for i = 0 to (t.width()) - 1 do
            for j = 0 to (t.height()) - 1 do
                if pieceToPlace.[j,i].IsSome then
                    do _board.[(i+(fst(t.offset))), (j+(snd(t.offset)))] <- pieceToPlace.[j,i]
                else
                    ()
        ()

    member val activeTetromino = activePiece with get, set
    member this.width = w
    member this.height = h
    member this.board with get() = _board
    member this.checkCollisionBoard (pos: position) (piece: bool[,]) : bool =
        let mutable offsetArray = []
        let x,y = pos
        for i = 0 to (piece |> Array2D.length2) - 1 do
            for j = 0 to (piece |> Array2D.length1) - 1 do
                if piece.[j,i] then
                    offsetArray <- offsetArray@[(i+x,j+y)]
                    ()
                else
                    ()
        offsetArray |> List.map (fun (x,y) -> x < 0 || x > 9 || y > 19) |> List.exists (fun x -> x = true)

    member this.checkCollisionPiece (pos: position) (piece: bool[,]) : bool =
        let mutable offsetArray = []
        let x,y = pos
        for i = 0 to (piece |> Array2D.length2) - 1 do
            for j = 0 to (piece |> Array2D.length1) - 1 do
                if piece.[j,i] then
                    offsetArray <- offsetArray@[(i+x,j+y)]
                    ()
                else
                    ()
        let mutable boardPieces = []
        for i = 0 to (this.board |> Array2D.length2) - 1 do
            for j = 0 to (this.board |> Array2D.length1) - 1 do
                if (this.board.[j,i]).IsSome then
                    boardPieces <- boardPieces@[j,i]
                    ()
                else
                    ()
        let res = offsetArray |> List.except boardPieces
        (res.Length <> 4)

    member this.performAction (act:Action) =
        let mutable is_down = false
        match this.activeTetromino with
        |None ->
            ()
        |Some(x)->
            let offset,img =
                match act with
                    |MoveRight -> x.moveRight()
                    |MoveLeft -> x.moveLeft()
                    |MoveDown ->
                        is_down <- true
                        x.moveDown()
                    |RotateRight -> x.rotateRight()
            if this.checkCollisionPiece offset img && is_down then
                this.put x |> ignore
                this.activeTetromino <- None
                ()
            elif this.checkCollisionBoard offset img && is_down then
                this.put x |> ignore
                this.activeTetromino <- None
                ()
            elif  this.checkCollisionPiece offset img then
                ()
            elif  this.checkCollisionBoard offset img then
                ()
            else
                x.setPiece offset img


type state = board
let draw (w: int) (h: int) (s: state) =
    let C = create w h
    s.board |> Array2D.iteri (
        fun i j v ->
        if v.IsSome then
            let col = v |> Option.get
            do setFillBox C ((fromColor col)) (i*30,j*30) ((i+1)*30, (j+1)*30)
        else
            do setFillBox C ((fromRgb(127,127,127))) ((i*30),(j*30)) ((i+1)*30, (j+1)*30)
        )
    if s.activeTetromino.IsSome then
        let activeNoOption = s.activeTetromino|>Option.get
        let pieceToPlace = (Array2D.create (activeNoOption.height()) (activeNoOption.width())(None))
        for i = 0 to (activeNoOption.width() - 1) do
            for j = 0 to (activeNoOption.height() - 1) do
                if activeNoOption.image.[j,i] then
                    pieceToPlace[j,i] <- Some activeNoOption.col
                else
                    pieceToPlace[j,i] <- None
        pieceToPlace|> Array2D.iteri (
            fun i j v ->
                if v.IsSome then
                    let col = v |> Option.get
                    let x,y = activeNoOption.offset
                    do setFillBox C ((fromColor col)) ((x+j)*30,(y+i)*30) ((x+j+1)*30, (y+i+1)*30)
                else
                ()
            )
    else
        ()
    for i = 1 to 9 do
        do setLine C (black) ((w/10)*i,0) ((w/10)*i,h)
    for i = 1 to 19 do
        do setLine C (black) (0,(h/20)*i) (w,(h/20)*i)
    C

let mutable score = 1
let react (s: state) (k: Canvas.key) : state option =
    if s.activeTetromino.IsNone then
        let newPiece = s.newPiece()
        if newPiece.IsSome then
            s.activeTetromino <- newPiece
            score <- score + 1
        else
            printfn"GAME OVER\nSCORE: %A" score
    else
        ()
    match getKey k with
        |UpArrow ->
            s.performAction RotateRight
            Some s
        |DownArrow ->
            s.performAction MoveDown
            Some s
        |LeftArrow ->
            s.performAction MoveLeft
            s.performAction MoveDown
            Some s
        |RightArrow ->
            s.performAction MoveRight
            s.performAction MoveDown
            Some s
        |_ ->
            Some s
let startState = (board(10,20))
startState.activeTetromino <- startState.newPiece()
