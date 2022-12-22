#r "nuget:DIKU.Canvas, 1.0.1"

open Canvas

#load "Tetris.fs"

open tetris

let startState = (board(10,20))
startState.activeTetromino <- startState.newPiece()
runApp "Tetris" 300 600 draw react startState
