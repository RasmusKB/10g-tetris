module tetris

type Color =
    | Yellow
    | Cyan
    | Blue
    | Orange
    | Red
    | Green
    | Purple

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
    member this.clone() =
        new tetromino((this.image),(this.col),(this.offset))
    member this.rotateRight () =
        let acc = Array2D.create (this.width()) (this.height()) false
        let rotated = Array2D.create (this.width()) (this.height()) false
        if this.height() = 1 || this.height() = 4 || (this.height() = 2 && this.width() = 2)  then
            this.image |> Array2D.iteri (fun i j v -> rotated[j,i] <- v)
        else
            this.image |> Array2D.iteri (fun i j v -> acc[j,i] <- v)
            acc |> Array2D.iteri (
                fun i j v ->
                    if acc |> Array2D.length2 = 2 then
                        if j = 0 then
                            rotated[i,j+1] <- v
                        else
                            rotated[i,j-1] <- v
                    else
                        if j = 0 then
                            rotated[i,j+2] <- v
                        elif j = 2 then
                            rotated[i,j-2] <- v
                        else
                            rotated[i,j] <- v
            )
        this.image <- rotated
        ()
    override this.ToString () =
        let replacestring (oldV:string) (newV:string) (image:string) =
            image.Replace(oldV, newV)
        let piecestring = sprintf"%A" this.image
        piecestring
        |> replacestring "[" ""
        |> replacestring "]" ""
        |> replacestring "false" "_"
        |> replacestring "true" "#"

let squareimage =
    Array2D.create 2 2 true

let straightimage =
    Array2D.create 1 4 true

let timage =
    let arr = Array2D.create 2 3 true
    arr[0,0] <- false
    arr[0,2] <- false
    arr

let limage =
    let arr = Array2D.create 2 3 true
    arr[0,0] <- false
    arr[0,1] <- false
    arr

let lmirrorimage =
    let arr = Array2D.create 2 3 true
    arr[0,2] <- false
    arr[0,1] <- false
    arr

let zimage =
    let arr = Array2D.create 2 3 true
    arr[0,0] <- false
    arr[1,2] <- false
    arr

let zmirrorimage =
    let arr = Array2D.create 2 3 true
    arr[1,0] <- false
    arr[0,2] <- false
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
    override this.ToString () =
        let replacestring (oldV:string) (newV:string) (image:string) =
            image.Replace(oldV, newV)
        let acc = Array2D.create h w None
        printfn"%A" acc
        for i = 0 to (this.board|> Array2D.length1) - 1 do
            for j = 0 to (this.board|> Array2D.length2) - 1 do
                acc[j,i] <- this.board[i,j]
        let boardstring = sprintf"%A" acc
        boardstring
        |> replacestring "[" ""
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
        match rnd1 with
        |0 -> new square(4,0) :> tetromino |> Some
        |1 -> new straight(3,0) :> tetromino |> Some
        |2 -> new l((3,0), false) :> tetromino |> Some
        |3 -> new l((3,0), true) :> tetromino |> Some
        |4 -> new t(3,0) :> tetromino |> Some
        |5 -> new z((3,0), false) :> tetromino |> Some
        |6 -> new z((3,0), true) :> tetromino |> Some
        |_ -> None
    member this.put (t: tetromino) : bool =
        let mutable acc = []
        let pieceToPlace = (Array2D.create (t.height()) (t.width())(None))
        for i = 0 to (t.width()) - 1 do
            for j = 0 to (t.height() - 1) do
                if this.board.[i,j] = None then
                    acc <- acc@[0]
                    if t.image.[j,i] then
                        pieceToPlace[j,i] <- Some t.col
                    else
                        pieceToPlace[j,i] <- None
                else
                    acc <- acc@[1]
        if not (acc|> List.contains 1) then
            for i = 0 to (t.width()) - 1 do
                for j = 0 to (t.height()) - 1 do
                    if pieceToPlace.[j,i].IsSome then
                        do _board.[(i+(fst(t.offset))), (j+(snd(t.offset)))] <- pieceToPlace.[j,i]
                    else
                        ()
            true
        else
            false
    member this.take() = activePiece
    member val activeTetromino = activePiece with get, set
    member this.width = w
    member this.height = h
    member this.board with get() = _board

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

let react (s: state) (k: Canvas.key) : state option =
    if s.activeTetromino.IsNone then
        s.activeTetromino <- s.newPiece()
    else
        ()
    match getKey k with
        |UpArrow ->
            let piece = s.activeTetromino |> Option.get
            piece.rotateRight()
            s.activeTetromino <- piece|>Some
            let piece1 = s.activeTetromino |> Option.get
            Some s
        |DownArrow ->
            let piece = s.activeTetromino |> Option.get
            piece.offset <- (fst(piece.offset),snd(piece.offset)+1)
            if (snd(piece.offset) + (piece.height())) >= 20 then
                s.put piece |> ignore
                s.activeTetromino <- s.newPiece()
            else
                s.activeTetromino <- piece|>Some
            Some s
        |LeftArrow ->
            let piece = s.activeTetromino |> Option.get
            if fst(piece.offset) = 0 then
                Some s
            elif (snd(piece.offset) + (piece.height())) >= 20 then
                s.put piece |> ignore
                s.activeTetromino <- s.newPiece()
                Some s
            else
                piece.offset <- (fst(piece.offset)-1,snd(piece.offset)+1)
                s.activeTetromino <- piece|>Some
                Some s
        |RightArrow ->
            let piece = s.activeTetromino |> Option.get
            if (fst(piece.offset)+piece.width()) = 10 then
                Some s
            elif (snd(piece.offset) + (piece.height())) >= 20 then
                s.put piece |> ignore
                s.activeTetromino <- s.newPiece()
                Some s
            else
                piece.offset <- (fst(piece.offset)+1,snd(piece.offset)+1)
                s.activeTetromino <- piece|>Some
                Some s
        |_ ->
            Some s

let startState = (board(10,20))
startState.activeTetromino <- startState.newPiece()
runApp "Tetris" 300 600 draw react startState
