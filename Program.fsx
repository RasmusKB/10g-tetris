﻿#r "nuget:DIKU.Canvas, 1.0"
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
    for i = 1 to 9 do
        do setLine C (black) ((w/10)*i,0) ((w/10)*i,h)
    for i = 1 to 19 do
        do setLine C (black) (0,(h/20)*i) (w,(h/20)*i)
    C


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

printfn"%A" squareimage
printfn"%A" straightimage
printfn"%A" timage
printfn"%A" limage
printfn"%A" lmirrorimage
printfn"%A" zimage
printfn"%A" zmirrorimage
type position = int*int
type tetromino (a: bool[,], c:Color, o:position) =
    override this.ToString () = sprintf"%A" this.image
    member this.clone() =
        new tetromino((this.image),(this.col),(this.offset))
    member this.rotateRight = 0
    member this.col = c
    member val image = a with get, set
    member this.offset = o
    member this.width = a|> Array2D.length1
    member this.height = a|> Array2D.length2

type square (o:position) =
    inherit tetromino (squareimage, Yellow, o)

type straight (o:position) =
    inherit tetromino (straightimage, Cyan, o)

type t (o:position) =
    inherit tetromino (timage, Purple,o)

type l (o:position, m:bool) =
    inherit tetromino ((if m then lmirrorimage else limage),(if m then Blue else Orange), o)

type z (o:position, m:bool) =
    inherit tetromino (zimage, Green, o)

let b = board(10, 20)
let C = draw 300 600 b
show C "testing"
