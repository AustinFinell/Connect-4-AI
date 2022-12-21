#|
Name: Austin Finell
Class: CSC240AA
Date: 11/9/2022
Program Name: Program11B.rkt
Program Description: A series of functions to control a connect 4 game.
|#

;Initialize AJFGame
(define AJFGame '())

;Initialize blank board state with first player
(define (AJFStartGame)
  (begin
    (set! AJFGame '(1
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0))
          )
    (display "Let's go ALGO!\n")
    #t
    )
  )

;Definitions to simplify references to player, board, and individual rows of game board
(define (AJFplayer)
  (car AJFGame))

(define (AJFboard)
  (cdr AJFGame))

(define (AJFrow1)
  (car (cdr (cdr (cdr (cdr (cdr (AJFboard))))))))

(define (AJFrow2)
  (car (cdr (cdr (cdr (cdr (AJFboard)))))))

(define (AJFrow3)
  (car (cdr (cdr (cdr (AJFboard))))))

(define (AJFrow4)
  (car (cdr (cdr (AJFboard)))))

(define (AJFrow5)
  (car (cdr (AJFboard))))

(define (AJFrow6)
  (car (AJFboard)))


(define (AJFOtherPlayer player)
  (if (= player 1)
      2
      1
      )
  )

;Displays Game board on screen
(define (AJFShowGame)
  (begin
    (display "Player: ")
    (display (AJFplayer)) (newline)
    (display (AJFrow6)) (newline)
    (display (AJFrow5)) (newline)
    (display (AJFrow4)) (newline)
    (display (AJFrow3)) (newline)
    (display (AJFrow2)) (newline)
    (display (AJFrow1)) (newline)
    #t
    )
  )

;Identify and set specific cell in a row
(define (AJFSetCol column list)
  (if (> column 1)
      (cons (car list) (AJFSetCol (- column 1) (cdr list)))
      (cons (AJFplayer) (cdr list))
      )
  )

;Identify and get specific cell in a row
(define (AJFGetCol column list)
  (if (> column 1)
      (AJFGetCol (- column 1) (cdr list))
      (car list)
      )
  )

;Return value of board at a given row and column
(define (AJFGetCell board column row)
  (if (< row 6)
      (AJFGetCell (cdr board) column (+ row 1))
      (AJFGetCol column (car board))
      )
  )

;Set value of board at a given row and column
(define (AJFSetCell board column row)
  (if (< row 6)
      (cons (car board) (AJFSetCell (cdr board) column (+ row 1)))
      (cons (AJFSetCol column (car board)) (cdr board))
      )
  )

;Creates a new board state based on given column
(define (AJFMove game column)
  (if (= (car game) 1)
              (cond
                ((= (AJFGetCell (cdr game) column 1) 0) (cons 2 (AJFSetCell (cdr game) column 1)))
                ((= (AJFGetCell (cdr game) column 2) 0) (cons 2 (AJFSetCell (cdr game) column 2)))
                ((= (AJFGetCell (cdr game) column 3) 0) (cons 2 (AJFSetCell (cdr game) column 3)))
                ((= (AJFGetCell (cdr game) column 4) 0) (cons 2 (AJFSetCell (cdr game) column 4)))
                ((= (AJFGetCell (cdr game) column 5) 0) (cons 2 (AJFSetCell (cdr game) column 5)))
                ((= (AJFGetCell (cdr game) column 6) 0) (cons 2 (AJFSetCell (cdr game) column 6)))
                )
              (cond
                ((= (AJFGetCell (cdr game) column 1) 0) (cons 1 (AJFSetCell (cdr game) column 1)))
                ((= (AJFGetCell (cdr game) column 2) 0) (cons 1 (AJFSetCell (cdr game) column 2)))
                ((= (AJFGetCell (cdr game) column 3) 0) (cons 1 (AJFSetCell (cdr game) column 3)))
                ((= (AJFGetCell (cdr game) column 4) 0) (cons 1 (AJFSetCell (cdr game) column 4)))
                ((= (AJFGetCell (cdr game) column 5) 0) (cons 1 (AJFSetCell (cdr game) column 5)))
                ((= (AJFGetCell (cdr game) column 6) 0) (cons 1 (AJFSetCell (cdr game) column 6)))
                )
          )
  )


;Sets gameboard to the board state from given column
(define (AJFMarkMove column)
  (begin
    (set! AJFGame (AJFMove AJFGame column))
    column
    )
  )

;Return true if given list is a connect 4
(define (AJFListCheck list)
  (if (and (> (car list) 0)
           (> (car (cdr list)) 0)
           (> (car (cdr (cdr list))) 0)
           (> (car (cdr (cdr (cdr list)))) 0))
      (if (= (car list)
             (car (cdr list))
             (car (cdr (cdr list)))
             (car (cdr (cdr (cdr list))))
             )
          #t #f)
      #f
      )
  )

;Return true if given list is a connect 4 for player 
(define (AJFPlayerCheck list player)
  (if (and (> (car list) 0)
           (> (car (cdr list)) 0)
           (> (car (cdr (cdr list))) 0)
           (> (car (cdr (cdr (cdr list)))) 0))
      (if (= (car list)
             (car (cdr list))
             (car (cdr (cdr list)))
             (car (cdr (cdr (cdr list))))
             player
             )
          #t #f)
      #f
      )
  )

(define (AJFCheckPlayerRow column row list count board player)
  (if (> count 0)
      (AJFCheckPlayerRow (+ column 1) row (cons (AJFGetCell board column row) list) (- count 1) board player)
      (AJFPlayerCheck list player)
      )
  )

(define (AJFHorizontalPlayerCheck row board player)
  (cond
    ((AJFCheckPlayerRow 1 row () 4 board player) #t)
    ((AJFCheckPlayerRow 2 row () 4 board player) #t)
    ((AJFCheckPlayerRow 3 row () 4 board player) #t)
    ((AJFCheckPlayerRow 4 row () 4 board player) #t)
    (#t #f)
    )
  )

(define (AJFCheckPlayerCol column row list count board player)
  (if (> count 0)
      (AJFCheckPlayerCol column (+ row 1) (cons (AJFGetCell board column row) list) (- count 1) board player)
      (AJFPlayerCheck list player)
      )
  )

(define (AJFVerticalPlayerCheck column board player)
  (cond
    ((AJFCheckPlayerCol column 1 () 4 board player) #t)
    ((AJFCheckPlayerCol column 2 () 4 board player) #t)
    ((AJFCheckPlayerCol column 3 () 4 board player) #t)
    (#t #f)
    )
  )

(define (AJFCheckPlayerDiagUp column row list count board player)
  (if (> count 0)
      (AJFCheckPlayerDiagUp (+ column 1) (+ row 1) (cons (AJFGetCell board column row) list) (- count 1) board player)
      (AJFPlayerCheck list player)
      )
  )

(define (AJFDiagUpPlayerCheck board player)
  (cond
    ((AJFCheckPlayerDiagUp 1 1 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 2 1 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 3 1 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 4 1 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 1 2 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 2 2 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 3 2 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 4 2 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 1 3 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 2 3 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 3 3 () 4 board player) #t)
    ((AJFCheckPlayerDiagUp 4 3 () 4 board player) #t)
    (#t #f)
    )
  )

(define (AJFCheckPlayerDiagDown column row list count board player)
  (if (> count 0)
      (AJFCheckPlayerDiagDown (+ column 1) (- row 1) (cons (AJFGetCell board column row) list) (- count 1) board player)
      (AJFPlayerCheck list player)
      )
  )

(define (AJFDiagDownPlayerCheck board player)
  (cond
    ((AJFCheckPlayerDiagDown 1 6 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 2 6 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 3 6 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 4 6 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 1 5 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 2 5 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 3 5 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 4 5 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 1 4 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 2 4 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 3 4 () 4 board player) #t)
    ((AJFCheckPlayerDiagDown 4 4 () 4 board player) #t)
    (#t #f)
    )
  )


(define (AJFConnectFourPlayer column row board player)
  (cond
    ((AJFHorizontalPlayerCheck row board player) #t)
    ((AJFVerticalPlayerCheck column board player) #t)
    ((AJFDiagUpPlayerCheck board player) #t)
    ((AJFDiagDownPlayerCheck board player) #t)
    (#t #f)
    )
  )


(define (AJFWinPlayer column board player)
  (cond
    ((> (AJFGetCell board column 6) 0)(AJFConnectFourPlayer column 6 board player))
    ((> (AJFGetCell board column 5) 0)(AJFConnectFourPlayer column 5 board player))
    ((> (AJFGetCell board column 4) 0)(AJFConnectFourPlayer column 4 board player))
    ((> (AJFGetCell board column 3) 0)(AJFConnectFourPlayer column 3 board player))
    ((> (AJFGetCell board column 2) 0)(AJFConnectFourPlayer column 2 board player))
    ((> (AJFGetCell board column 1) 0)(AJFConnectFourPlayer column 1 board player))
    (#t #f)
    )
  )


;Checks a single row of pieces for a connect 4
(define (AJFCheckRow column row list count board)
  (if (> count 0)
      (AJFCheckRow (+ column 1) row (cons (AJFGetCell board column row) list) (- count 1) board)
      (AJFListCheck list)
      )
  )

;Checks given row for connect 4's
(define (AJFHorizontalCheck row board)
  (cond
    ((AJFCheckRow 1 row () 4 board) #t)
    ((AJFCheckRow 2 row () 4 board) #t)
    ((AJFCheckRow 3 row () 4 board) #t)
    ((AJFCheckRow 4 row () 4 board) #t)
    (#t #f)
    )
  )

;Checks a vertical column of pieces for a connect 4
(define (AJFCheckCol column row list count board)
  (if (> count 0)
      (AJFCheckCol column (+ row 1) (cons (AJFGetCell board column row) list) (- count 1) board)
      (AJFListCheck list)
      )
  )

;Checks given column for connect 4's
(define (AJFVerticalCheck column board)
  (cond
    ((AJFCheckCol column 1 () 4 board) #t)
    ((AJFCheckCol column 2 () 4 board) #t)
    ((AJFCheckCol column 3 () 4 board) #t)
    (#t #f)
    )
  )

;Check a diagonal upward for connect 4
(define (AJFCheckDiagUp column row list count board)
  (if (> count 0)
      (AJFCheckDiagUp (+ column 1) (+ row 1) (cons (AJFGetCell board column row) list) (- count 1) board)
      (AJFListCheck list)
      )
  )

;Check all diagonal up connect 4's
(define (AJFDiagUpCheck board)
  (cond
    ((AJFCheckDiagUp 1 1 () 4 board) #t)
    ((AJFCheckDiagUp 2 1 () 4 board) #t)
    ((AJFCheckDiagUp 3 1 () 4 board) #t)
    ((AJFCheckDiagUp 4 1 () 4 board) #t)
    ((AJFCheckDiagUp 1 2 () 4 board) #t)
    ((AJFCheckDiagUp 2 2 () 4 board) #t)
    ((AJFCheckDiagUp 3 2 () 4 board) #t)
    ((AJFCheckDiagUp 4 2 () 4 board) #t)
    ((AJFCheckDiagUp 1 3 () 4 board) #t)
    ((AJFCheckDiagUp 2 3 () 4 board) #t)
    ((AJFCheckDiagUp 3 3 () 4 board) #t)
    ((AJFCheckDiagUp 4 3 () 4 board) #t)
    (#t #f)
    )
  )

;check a diagonal downward for a connect 4
(define (AJFCheckDiagDown column row list count board)
  (if (> count 0)
      (AJFCheckDiagDown (+ column 1) (- row 1) (cons (AJFGetCell board column row) list) (- count 1) board)
      (AJFListCheck list)
      )
  )

;check all downward diagonals for connect 4's
(define (AJFDiagDownCheck board)
  (cond
    ((AJFCheckDiagDown 1 6 () 4 board) #t)
    ((AJFCheckDiagDown 2 6 () 4 board) #t)
    ((AJFCheckDiagDown 3 6 () 4 board) #t)
    ((AJFCheckDiagDown 4 6 () 4 board) #t)
    ((AJFCheckDiagDown 1 5 () 4 board) #t)
    ((AJFCheckDiagDown 2 5 () 4 board) #t)
    ((AJFCheckDiagDown 3 5 () 4 board) #t)
    ((AJFCheckDiagDown 4 5 () 4 board) #t)
    ((AJFCheckDiagDown 1 4 () 4 board) #t)
    ((AJFCheckDiagDown 2 4 () 4 board) #t)
    ((AJFCheckDiagDown 3 4 () 4 board) #t)
    ((AJFCheckDiagDown 4 4 () 4 board) #t)
    (#t #f)
    )
  )

;Check for connect 4's given the last cell placed
(define (AJFConnectFour column row board)
  (cond
    ((AJFHorizontalCheck row board) #t)
    ((AJFVerticalCheck column board) #t)
    ((AJFDiagUpCheck board) #t)
    ((AJFDiagDownCheck board) #t)
    (#t #f)
    )
  )

;Check if a move on given board will win
(define (AJFWin column board)
  (cond
    ((> (AJFGetCell board column 6) 0)(AJFConnectFour column 6 board))
    ((> (AJFGetCell board column 5) 0)(AJFConnectFour column 5 board))
    ((> (AJFGetCell board column 4) 0)(AJFConnectFour column 4 board))
    ((> (AJFGetCell board column 3) 0)(AJFConnectFour column 3 board))
    ((> (AJFGetCell board column 2) 0)(AJFConnectFour column 2 board))
    ((> (AJFGetCell board column 1) 0)(AJFConnectFour column 1 board))
    (#t #f)
    )
  )

;Check if there was a winning move made given the column of last turn
(define (AJFWinP column)
  (cond
    ((> (AJFGetCell (AJFboard) column 6) 0)(AJFConnectFour column 6 (AJFboard)))
    ((> (AJFGetCell (AJFboard) column 5) 0)(AJFConnectFour column 5 (AJFboard)))
    ((> (AJFGetCell (AJFboard) column 4) 0)(AJFConnectFour column 4 (AJFboard)))
    ((> (AJFGetCell (AJFboard) column 3) 0)(AJFConnectFour column 3 (AJFboard)))
    ((> (AJFGetCell (AJFboard) column 2) 0)(AJFConnectFour column 2 (AJFboard)))
    ((> (AJFGetCell (AJFboard) column 1) 0)(AJFConnectFour column 1 (AJFboard)))
    (#t #f)
    )
  )
  
;Check if given move will win
(define (AJFWillWinP column)
  (if (AJFLegalMoveP column)
      (if (AJFWin column (cdr (AJFMove AJFGame column)))
          #t
          #f
      )
      #f
    )
  )

;Recursively choose a legal random column if num is not legal
(define (AJFLegalRandCol num)
  (if (AJFLegalMoveP num)
      num
      (AJFLegalRandCol (+ (random 7) 1))
      )
  )

;Return whether a move is legal or not
(define (AJFLegalMoveP column)
  (if (or (< column 1) (> column 7))
      #f
      (if (> (AJFGetCell (AJFboard) column 6) 0)
          #f
          #t
          )
      )
  )

(define (AJFLegalMove column board)
  (if (or (< column 1) (> column 7))
      #f
      (if (> (AJFGetCell board column 6) 0)
          #f
          #t
          )
      )
  )

(define (AJFNoMoves board)
  (not (or (AJFLegalMove 1 board)
           (AJFLegalMove 2 board)
           (AJFLegalMove 3 board)
           (AJFLegalMove 4 board)
           (AJFLegalMove 5 board)
           (AJFLegalMove 6 board)
           (AJFLegalMove 7 board)
           )
      )
  )

;count empty spaces in list
(define (AJFListCount0 list count)
  (if (null? list)
      count
      (if (= (car list) 0)
          (AJFListCount0 (cdr list) (+ count 1))
          (AJFListCount0 (cdr list) count)
          )
      )
  )

;count 1's in a list
(define (AJFListCount1 list count)
  (if (null? list)
      count
      (if (= (car list) 1)
          (AJFListCount1 (cdr list) (+ count 1))
          (AJFListCount1 (cdr list) count)
          )
      )
  )

;count 2's in a list
(define (AJFListCount2 list count)
  (if (null? list)
      count
      (if (= (car list) 2)
          (AJFListCount2 (cdr list) (+ count 1))
          (AJFListCount2 (cdr list) count)
          )
      )
  )

;Create a list of count size beginning at given column and row of board horizontally
(define (AJFRowList board column row list count)
  (if (> count 0)
      (AJFRowList board (+ column 1) row (cons (AJFGetCell board column row) list) (- count 1))
      list
      )
  )

;Create a list of count size beginning at given column and row of board vertically
(define (AJFColList board column row list count)
  (if (> count 0)
      (AJFColList board column (+ row 1) (cons (AJFGetCell board column row) list) (- count 1))
      list
      )
  )

;Create a list of count size beginning at given column and row of board diagonally up
(define (AJFDiagUpList board column row list count)
  (if (> count 0)
      (AJFDiagUpList board (+ column 1) (+ row 1) (cons (AJFGetCell board column row) list) (- count 1))
      list
      )
  )

;Create a list of count size beginning at given column and row of board diagonally down
(define (AJFDiagDownList board column row list count)
  (if (> count 0)
      (AJFDiagDownList board (+ column 1) (- row 1) (cons (AJFGetCell board column row) list) (- count 1))
      list
      )
  )

;Check if a given player won in a given list
(define (AJFPlayerWon list player)
  (if (and (> (car list) 0)
           (> (car (cdr list)) 0)
           (> (car (cdr (cdr list))) 0)
           (> (car (cdr (cdr (cdr list)))) 0))
      (if (= (car list)
             (car (cdr list))
             (car (cdr (cdr list)))
             (car (cdr (cdr (cdr list))))
             player)
          #t #f)
      #f
      )
  )

;Recursively Look through list to see if a set was a win for the player
(define (AJFPlayerWonR list player)
  (if (null? list)
      #f
      (if (AJFPlayerWon (car list) player)
          #t
          (AJFPlayerWonR (cdr list) player)
          )
      )
  )

;Check all win variations for player
(define (AJFWon board player)
  (or (AJFPlayerWonR (AJFRowSet board) player) (AJFPlayerWonR (AJFColSet board) player) (AJFPlayerWonR (AJFDiagSet board) player))
  )
  

;list all sets of 4 on rows on board
(define (AJFRowSet board)
    (cons (AJFRowList board 1 1 () 4)
    (cons (AJFRowList board 2 1 () 4)
    (cons (AJFRowList board 3 1 () 4)
    (cons (AJFRowList board 4 1 () 4)
    (cons (AJFRowList board 1 2 () 4)
    (cons (AJFRowList board 2 2 () 4)
    (cons (AJFRowList board 3 2 () 4)
    (cons (AJFRowList board 4 2 () 4)
    (cons (AJFRowList board 1 3 () 4)
    (cons (AJFRowList board 2 3 () 4)
    (cons (AJFRowList board 3 3 () 4)
    (cons (AJFRowList board 4 3 () 4)
    (cons (AJFRowList board 1 4 () 4)
    (cons (AJFRowList board 2 4 () 4)
    (cons (AJFRowList board 3 4 () 4)
    (cons (AJFRowList board 4 4 () 4)
    (cons (AJFRowList board 1 5 () 4)
    (cons (AJFRowList board 2 5 () 4)
    (cons (AJFRowList board 3 5 () 4)
    (cons (AJFRowList board 4 5 () 4)
    (cons (AJFRowList board 1 6 () 4)
    (cons (AJFRowList board 2 6 () 4)
    (cons (AJFRowList board 3 6 () 4)
    (cons (AJFRowList board 4 6 () 4) ()))))))))))))))))))))))))
  )

;list all sets of 4 on columns on board
(define (AJFColSet board)
    (cons (AJFColList board 1 1 () 4)
    (cons (AJFColList board 1 2 () 4)
    (cons (AJFColList board 1 3 () 4)
    (cons (AJFColList board 2 1 () 4)
    (cons (AJFColList board 2 2 () 4)
    (cons (AJFColList board 2 3 () 4)
    (cons (AJFColList board 3 1 () 4)
    (cons (AJFColList board 3 2 () 4)
    (cons (AJFColList board 3 3 () 4)
    (cons (AJFColList board 4 1 () 4)
    (cons (AJFColList board 4 2 () 4)
    (cons (AJFColList board 4 3 () 4)
    (cons (AJFColList board 5 1 () 4)
    (cons (AJFColList board 5 2 () 4)
    (cons (AJFColList board 5 3 () 4)
    (cons (AJFColList board 6 1 () 4)
    (cons (AJFColList board 6 2 () 4)
    (cons (AJFColList board 6 3 () 4)
    (cons (AJFColList board 7 1 () 4)
    (cons (AJFColList board 7 2 () 4)
    (cons (AJFColList board 7 3 () 4) ())))))))))))))))))))))
  )

;list all sets of 4 on diagonals on board
(define (AJFDiagSet board)
    (cons (AJFDiagUpList board 1 1 () 4)
    (cons (AJFDiagUpList board 2 1 () 4)
    (cons (AJFDiagUpList board 3 1 () 4)
    (cons (AJFDiagUpList board 4 1 () 4)
    (cons (AJFDiagUpList board 1 2 () 4)
    (cons (AJFDiagUpList board 2 2 () 4)
    (cons (AJFDiagUpList board 3 2 () 4)
    (cons (AJFDiagUpList board 4 2 () 4)
    (cons (AJFDiagUpList board 1 3 () 4)
    (cons (AJFDiagUpList board 2 3 () 4)
    (cons (AJFDiagUpList board 3 3 () 4)
    (cons (AJFDiagUpList board 4 3 () 4)
    (cons (AJFDiagDownList board 1 6 () 4)
    (cons (AJFDiagDownList board 2 6 () 4)
    (cons (AJFDiagDownList board 3 6 () 4)
    (cons (AJFDiagDownList board 4 6 () 4)
    (cons (AJFDiagDownList board 1 5 () 4)
    (cons (AJFDiagDownList board 2 5 () 4)
    (cons (AJFDiagDownList board 3 5 () 4)
    (cons (AJFDiagDownList board 4 5 () 4)
    (cons (AJFDiagDownList board 1 4 () 4)
    (cons (AJFDiagDownList board 2 4 () 4)
    (cons (AJFDiagDownList board 3 4 () 4)
    (cons (AJFDiagDownList board 4 4 () 4) ()))))))))))))))))))))))))
  )

;Returns score given to row of 4 slots with regards to the player
(define (AJFScoreHorizontal list player)
  (if (= player 2)
      (cond
        ((= (AJFListCount2 list 0) 4) 100000000)
        ((and (= (AJFListCount2 list 0) 3) (= (AJFListCount0 list 0) 1)) 12)
        ((and (= (AJFListCount2 list 0) 2) (= (AJFListCount0 list 0) 2)) 8)
        ((= (AJFListCount1 list 0) 4) -10000)
        ((and (= (AJFListCount1 list 0) 3) (= (AJFListCount0 list 0) 1)) -10000)
        ((and (= (AJFListCount1 list 0) 2) (= (AJFListCount0 list 0) 2)) -16)
        (#t 0)
       )
      (cond
        ((= (AJFListCount1 list 0) 4) 100000000)
        ((and (= (AJFListCount1 list 0) 3) (= (AJFListCount0 list 0) 1)) 12)
        ((and (= (AJFListCount1 list 0) 2) (= (AJFListCount0 list 0) 2)) 8)
        ((= (AJFListCount2 list 0) 4) -10000)
        ((and (= (AJFListCount2 list 0) 3) (= (AJFListCount0 list 0) 1)) -10000)
        ((and (= (AJFListCount2 list 0) 2) (= (AJFListCount0 list 0) 2)) -16)
        (#t 0)
       )
      )
  )

;Returns score given to column of 4 slots with regards to the player
(define (AJFScoreVertical list player)
  (if (= player 2)
      (cond
        ((= (AJFListCount2 list 0) 4) 100000000)
        ((and (= (AJFListCount2 list 0) 3) (= (AJFListCount0 list 0) 1)) 8)
        ((and (= (AJFListCount2 list 0) 2) (= (AJFListCount0 list 0) 2)) 4)
        ((= (AJFListCount1 list 0) 4) -10000)
        ((and (= (AJFListCount1 list 0) 3) (= (AJFListCount0 list 0) 1)) -10000)
        ((and (= (AJFListCount1 list 0) 2) (= (AJFListCount0 list 0) 2)) -16)
        (#t 0)
       )
      (cond
        ((= (AJFListCount1 list 0) 4) 100000000)
        ((and (= (AJFListCount1 list 0) 3) (= (AJFListCount0 list 0) 1)) 8)
        ((and (= (AJFListCount1 list 0) 2) (= (AJFListCount0 list 0) 2)) 4)
        ((= (AJFListCount2 list 0) 4) -10000)
        ((and (= (AJFListCount2 list 0) 3) (= (AJFListCount0 list 0) 1)) -10000)
        ((and (= (AJFListCount2 list 0) 2) (= (AJFListCount0 list 0) 2)) -16)
        (#t 0)
       )
      )
  )


;Returns score given to diagonal of 4 slots with regards to the player
(define (AJFScoreDiagonal list player)
  (if (= player 2)
      (cond
        ((= (AJFListCount2 list 0) 4) 100000000)
        ((and (= (AJFListCount2 list 0) 3) (= (AJFListCount0 list 0) 1)) 12)
        ((and (= (AJFListCount2 list 0) 2) (= (AJFListCount0 list 0) 2)) 10)
        ((= (AJFListCount1 list 0) 4) -10000)
        ((and (= (AJFListCount1 list 0) 3) (= (AJFListCount0 list 0) 1)) -10000)
        ((and (= (AJFListCount1 list 0) 2) (= (AJFListCount0 list 0) 2)) -12)
        (#t 0)
       )
      (cond
        ((= (AJFListCount1 list 0) 4) 100000000)
        ((and (= (AJFListCount1 list 0) 3) (= (AJFListCount0 list 0) 1)) 12)
        ((and (= (AJFListCount1 list 0) 2) (= (AJFListCount0 list 0) 2)) 10)
        ((= (AJFListCount2 list 0) 4) -10000)
        ((and (= (AJFListCount2 list 0) 3) (= (AJFListCount0 list 0) 1)) -10000)
        ((and (= (AJFListCount2 list 0) 2) (= (AJFListCount0 list 0) 2)) -12)
        (#t 0)
       )
      )
  )

;Adds score for all rows
(define (AJFScoreRows list player score)
  (if (null? list)
      score
      (AJFScoreRows (cdr list) player (+ score (AJFScoreHorizontal (car list) player)))
      )
  )

;Adds score for all columns
(define (AJFScoreCols list player score)
  (if (null? list)
      score
      (AJFScoreCols (cdr list) player (+ score (AJFScoreVertical (car list) player)))
      )
  )

;Adds score for all diagonals
(define (AJFScoreDiags list player score)
  (if (null? list)
      score
      (AJFScoreDiags (cdr list) player (+ score (AJFScoreDiagonal (car list) player)))
      )
  )

;Returns total score for given board for the given player
(define (AJFScoreBoard player board)
  (+ (AJFScoreRows (AJFRowSet board) player 0) (AJFScoreCols (AJFColSet board) player 0) (AJFScoreDiags (AJFDiagSet board) player 0))
  )

(define (AJFTestMove column)
  (AJFScoreBoard (AJFplayer) (cdr (AJFMove AJFGame column)))
  )

(define (AJFCheckMove game column)
  (AJFScoreBoard (car game) (cdr (AJFMove game column)))
  )

(define (AJFScoresListMax game columns list)
  (if (< columns 1)
      list
      (if (AJFLegalMoveP columns)
          (if (= columns 4)
              (AJFScoresListMax game (- columns 1) (cons (+ (AJFCheckMove game columns) 4) list))
              (AJFScoresListMax game (- columns 1) (cons (AJFCheckMove game columns) list))
              )
          (AJFScoresListMax game (- columns 1) (cons -99999999999999 list))
          )
      )
  )

(define (AJFListedCols)
  '(1 2 3 4 5 6 7)
  )

(define (AJFReplaceItem list item newItem)
  (if (= (car list) item)
      (cons newItem (cdr list))
      (cons (car list) (AJFReplaceItem (cdr list) item newItem))
      )
  )

(define (AJFReplaceMax list)
  (AJFReplaceItem list (AJFMaxScore list -9999999999) -9999999999)
  )

(define (AJFMaxCol list colsList score bestCol)
  (if (null? list)
      bestCol
      (if (> (car list) score)
          (AJFMaxCol (cdr list) (cdr colsList) (car list) (car colsList))
          (AJFMaxCol (cdr list) (cdr colsList) score bestCol)
          )
      )
  )

(define (AJFMaxScore list score)
  (if (null? list)
      score
      (if (> (car list) score)
          (AJFMaxScore (cdr list) (car list))
          (AJFMaxScore (cdr list) score)
          )
      )
  )

(define (AJFMinCol list colsList score bestCol)
  (if (null? list)
      bestCol
      (if (< (car list) score)
          (AJFMinCol (cdr list) (cdr colsList) (car list) (car colsList))
          (AJFMinCol (cdr list) (cdr colsList) score bestCol)
          )
      )
  )

(define (AJFMinScore list score)
  (if (null? list)
      score
      (if (< (car list) score)
          (AJFMinScore (cdr list) (car list))
          (AJFMinScore (cdr list) score)
          )
      )
  )

(define (AJFSubScores scores1 scores2)
  (if (null? (cdr scores1))
      (cons (- (car scores1) (car scores2)) ())
      (cons (- (car scores1) (car scores2))(AJFSubScores (cdr scores1) (cdr scores2)))
      )
  )

(define (AJFAddScores scores1 scores2)
  (if (null? (cdr scores1))
      (cons (+ (car scores1) (car scores2)) ())
      (cons (+ (car scores1) (car scores2))(AJFAddScores (cdr scores1) (cdr scores2)))
      )
  )

(define (AJFWillWin game column)
  (if (AJFWin column (cdr (AJFMove game column)))
      #t
      #f
      )
  )

(define (AJFWillWinPlayer game column player)
  (if (AJFWinPlayer column (cdr (AJFMove game column)) player)
      #t
      #f
      )
  )




;checks if other player will win on best move
(define (AJFWinningMove game colsList player)
  (if (null? colsList)
      #f
      (if (AJFLegalMove (car colsList) (cdr game))
          (if (AJFWillWinPlayer game (car colsList) player)
              #t
              (AJFWinningMove game (cdr colsList) player)
          )
          (AJFWinningMove game (cdr colsList) player)
          )
      )
  )

(define (AJFListLegalMoves board colList)
  (if (null? (cdr colList))
      (if (AJFLegalMove (car colList) board)
          (cons (car colList) ())
          ()
          )
      (if (AJFLegalMove (car colList) board)
          (cons (car colList) (AJFListLegalMoves board (cdr colList)))
          (AJFListLegalMoves board (cdr colList))
          )
      )
  )



;AI Implementation
(define (AJFMakeMove)
  (cond
    ((AJFWillWinP 1) 1)
    ((AJFWillWinP 2) 2)
    ((AJFWillWinP 3) 3)
    ((AJFWillWinP 4) 4)
    ((AJFWillWinP 5) 5)
    ((AJFWillWinP 6) 6)
    ((AJFWillWinP 7) 7)
    (#t (if (AJFWinningMove (AJFMove AJFGame (AJFMaxCol (AJFScoresListMax AJFGame 7 ()) (AJFListedCols) -10000000 (AJFLegalRandCol (+ (random 7) 1))))
                          (AJFListedCols)
                          (car AJFGame))
          (AJFMaxCol (AJFReplaceMax (AJFScoresListMax AJFGame 7 ())) (AJFListedCols) -10000000 (AJFLegalRandCol (+ (random 7) 1)))
          (AJFMaxCol (AJFScoresListMax AJFGame 7 ()) (AJFListedCols) -10000000 (AJFLegalRandCol (+ (random 7) 1)))
          ))
    )
  )

(AJFStartGame)
(AJFShowGame)

(define (x col)
  (AJFMarkMove col)
  (AJFMarkMove (AJFMakeMove))
  (AJFShowGame)
  )
