#r "nuget:DIKU.Canvas, 1.0.1"

open Canvas

#load "Tetris.fs"

open tetris

runApp "Tetris" 300 600 draw react startState

type Assert () =
    static let mutable failedTests = 0
    member this.test str testobj testval test =
        if test testobj testval = true then
            printfn"Passed: %s" (str)
        else
            failedTests <- failedTests + 1
            printfn"Failed: %s" str
    member this.totalFailed = failedTests
let a = new Assert()
printfn"_______________TETROMINO TEST_______________"
let testpiece1 = new square(0,0)
let testpiece2 = new straight(0,0)
let rotatedStraight =
    let mutable arr = Array2D.create 4 4 false
    arr[0,2] <- true
    arr[1,2] <- true
    arr[2,2] <- true
    arr[3,2] <- true
    arr
a.test("this.image returns the image") (testpiece1.image) (squareimage) (=)
a.test("this.col returns the color") (testpiece1.col) (Yellow) (=)
a.test("this.offset returns the offset") (testpiece1.offset) ((0,0)) (=)
a.test("this.width returns the width") (testpiece1.width()) (2) (=)
a.test("this.height returns the height") (testpiece1.height()) (2) (=)
a.test("this.ToString() returns a string") (testpiece1.ToString()) ("#; #\n #; #") (=)
a.test("this.moveRight() moves offset to the right") (testpiece1.moveRight()) ((1,0), testpiece1.image) (=)
a.test("this.moveLeft() moves offset to the left") (testpiece1.moveLeft()) ((-1,0), testpiece1.image) (=)
a.test("this.moveDown() moves offset to the right") (testpiece1.moveDown()) ((0,1), testpiece1.image) (=)
a.test("this.rotateRight() moves offset to the right") (testpiece2.rotateRight()) ((0,0), rotatedStraight) (=)
testpiece2.setPiece (10,10) rotatedStraight
a.test("this.setPiece updates the offset") (testpiece2.offset) ((10,10)) (=)
a.test("this.setPiece updates the image") (testpiece2.image) (rotatedStraight) (=)

printfn"_______________BOARD TEST_______________"
let testboard1 = new board(10,20)
let testnewPiece = testboard1.activeTetromino
a.test("this.board returns the board") (testboard1.board) ((Array2D.create 10 20 None)) (=)
testboard1.activeTetromino <- testboard1.newPiece()
a.test("this.newPiece() returns a random piece") (testnewPiece) (testboard1.activeTetromino) (<>)
let testboard2 = new board(10,20)
testboard1.put ((testboard1.newPiece()|>Option.get))
a.test("this.put() puts the piece on the board") (testboard1.board) (testboard2.board) (<>)
a.test("this.activeTetromino returns the current active piece") (testboard2.activeTetromino) (None) (=)
a.test("this.width returns the width") (testboard1.width) (10) (=)
a.test("this.height returns the height") (testboard1.height) (20) (=)
let testColPiece1 = new square(5,5)
let testColPiece2 = new square(-5,5)
a.test("this.checkCollisionBoard checks if piece is outside the board with offset (0,0)") (testboard1.checkCollisionBoard testColPiece1.offset testColPiece1.image) (false) (=)
a.test("this.checkCollisionBoard checks if piece is outside the board with offset (-5,5)") (testboard1.checkCollisionBoard testColPiece2.offset testColPiece2.image) (true) (=)
testboard2.put(testColPiece1)
a.test("this.checkCollisionPiece checks if piece is colliding with piece") (testboard2.checkCollisionPiece (6,5) squareimage) (true) (=)
a.test("this.checkCollisionPiece checks if piece is colliding with piece") (testboard2.checkCollisionPiece (0,5) squareimage) (true) (<>)
let testboard3 = new board(10,20)
let testPerform = new square(7,17)
testboard3.activeTetromino <- (testPerform:> tetromino)|>Some
testboard3.performAction MoveRight
let performActionPiece = testboard3.activeTetromino
a.test("this.performAction performs action MoveRight") ((performActionPiece|>Option.get).offset) ((8,17)) (=)
testboard3.performAction MoveRight
a.test("this.performAction Does not perform action if collision with board happens") ((performActionPiece|>Option.get).offset) ((8,17)) (=)
testboard3.performAction MoveLeft
a.test("this.performAction performs action MoveLeft") ((performActionPiece|>Option.get).offset) ((7,17)) (=)
testboard3.performAction MoveDown
a.test("this.performAction performs action MoveDown") ((performActionPiece|>Option.get).offset) ((7,18)) (=)
testboard3.put(new square(5,18))
testboard3.performAction MoveLeft
a.test("this.performAction Does nothing if piece if moved sideways into another piece") ((performActionPiece|>Option.get).offset) ((7,18)) (=)
testboard3.performAction MoveDown
a.test("this.performAction Puts piece on the board if collision with bottom board happens") (testboard3.activeTetromino) (None) (=)
testboard3.activeTetromino <- ((new square(5,16)):>tetromino|> Some)
testboard3.performAction MoveDown
a.test("this.performAction Puts piece on top of other piece if collided from the top") (testboard3.activeTetromino) (None) (=)

printfn"_______________BLACKBOX TESTING EXAMPLE________________"
let blackBoxBoard1 = new board(10,20)
let bPLeftPiece = new l((0,0),false)
let colx,coly = bPLeftPiece.moveLeft()
let bPLeftCol = blackBoxBoard1.checkCollisionBoard colx coly
a.test("this.checkCollisionBoard edgecase test: Collides with left side of board") (bPLeftCol) (true) (=)
let bPRightPiece = new l((7,0),false)
let colrx,colry = bPRightPiece.moveRight()
let bPRightCol = blackBoxBoard1.checkCollisionBoard colrx colry
a.test("this.checkCollisionBoard edgecase test: Collides with right side of board") (bPRightCol) (true) (=)
let bPDownPiece = new l((0,18),false)
let coldx,coldy = bPDownPiece.moveDown()
let bPDownCol = blackBoxBoard1.checkCollisionBoard coldx coldy
a.test("this.checkCollisionBoard edgecase test: Collides with bottom of board") (bPDownCol) (true) (=)
printfn"Total passed tests: %A" a.totalFailed
