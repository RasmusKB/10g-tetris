#r "nuget:DIKU.Canvas, 1.0"
open Canvas

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

type board (w: int , h: int) =
    let _board = Array2D.create w h None
    do _board .[0 ,1] <- Some Green
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
    C
type position = int*int
type tetromino =
/// The constructor with its initial shape , its final color, and its inital offset
    new: a: bool[,] * c: Color * o: position -> tetromino
/// Make a string representation of this piece
    override ToString: unit -> string
/// Make a deep copy of this piece
    member clone: unit -> tetromino
/// Rotates the piece 90 degrees clock -wise such that its left -top offset is maintained
    member rotateRight: unit -> unit
/// The piece 'color
    member col: Color
/// The present height of the shape
    member height: int
/// The piece 'present shape
    member image: bool[,]
/// The piece 'present offset
    member offset: position
/// The present width of the shape
    member width: int
let b = board(10, 20)
let C = draw 300 600 b
show C "testing"
