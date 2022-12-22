module tetris

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

val fromColor: c: Color -> Canvas.color

type position = int*int
type tetromino =
    new: a: bool[,] * c: Color * o: position -> tetromino
    member image: bool[,]
    member col: Color
    member offset: position
    member width: unit -> int
    member height: unit -> int
    override ToString: unit -> string
    member moveRight: unit -> ((int*int)*bool[,])
    member moveLeft: unit -> ((int*int)*bool[,])
    member moveDown: unit -> ((int*int)*bool[,])
    member rotateRight: unit -> ((int*int)*bool[,])
    member setPiece: pos: position -> img: bool[,] -> unit


type square =
    inherit tetromino
    new: o: position -> square
type straight =
    inherit tetromino
    new: o: position -> straight
type t =
    inherit tetromino
    new: o: position -> t
type l =
    inherit tetromino
    new: o: position * m:bool -> l
type z =
    inherit tetromino
    new: o: position * m:bool -> z


type board =
    new: w: int * h: int -> board
    override ToString: unit -> string
    member newPiece: unit -> tetromino option
    member put: t: tetromino -> unit
    member activeTetromino: tetromino option
    member width: int
    member height: int
    member board: Color option [,]
    member checkCollisionBoard: pos: position -> piece: bool[,] -> bool
    member checkCollisionPiece: pos: position -> piece: bool[,] -> bool
    member performAction: act:Action -> unit

type state = board

val draw: w: int -> h: int -> state -> Canvas.canvas

val react: s: state -> k: Canvas.key -> state option
