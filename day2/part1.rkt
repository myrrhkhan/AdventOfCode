#lang racket

(require racket/file)

;; Purpose: Solve problem
;; Input: Path to input file
(define (main path-to-input)
  (readfile->sum path-to-input))

;; Purpose: Read file into a list of strings and return the sum
;; Input: Path to input file
(define (readfile->sum path-to-input)
  (lines->sum (file->lines path-to-input)))

;; Purpose: Convert a list of strings into a sum based on game conditions
;; Input: List of strings, Running total (default value 0) (ex: (list "Game 1:..." "Game 2:..."))
(define (lines->sum lines (running-total 0))
  (cond
    [(empty? lines) running-total]
    [else
      (let* ([line (first lines)] ; first line
             [linesplit (string-split line ": ")] ; convert "Game 1: 1 red" into "Game 1" "1 red"
             [gameid (string->number (second (string-split (first linesplit) " ")))] ; convert "Game 1" into "1"
             [trimmed-line (rest linesplit)] ; convert "Game 1: 1 red, 4 blue; 1 red..." into "1 red, 4 blue" etc 
             [sets (string-split (first trimmed-line) "; ")] ; split "1 red; 4 blue" into ("1 red" "4 blue")
             [valid? (sets->boolean sets)]) ; see if sets are a part of a valid game
        (if valid?
            (lines->sum (rest lines) (+ running-total gameid)) ; check rest of lines and add gameid to running total
            (lines->sum (rest lines) running-total)) ; check rest of lines
        )
      ]
    )
  )

;; Purpose: Check if a game is possible based on color limits
;; Input: List of strings representing subsets of cubes, ex:
;; ("3 blue, 4 red" "1 red, 2 green")
(define (sets->boolean sets)
  (cond
    [(empty? sets) #t]
    [else
      (define set (string-split (first sets) ", ")) ;; --> turns above example into ("3 blue" "4 red") etc.
      (if (indivset->boolean set)
          (sets->boolean (rest sets))
          #f)
    ]
    )
  )

;; Purpose: check if an individual round is possible
;; Input: individual set (listof string) -> boolean
(define (indivset->boolean set)
  (cond
    [(empty? set) #t]
    [else
     (define cube (first set)) ; takes first in "3 blue, 4 red"
     (define numcolor (string-split cube " ")) ; "3 blue" -> ("3" "blue")
     (define num (first numcolor))
     (define color (second numcolor))
     (define color-limit (get-color-limit color))
       (if (<= (string->number num) color-limit)
            (indivset->boolean (rest set))
            #f)
    ])
  
  )

;; Purpose: Get the color limit based on the color
;; Input: Color (string)
(define (get-color-limit color)
  (cond
    [(string=? color "red") 12]
    [(string=? color "green") 13]
    [(string=? color "blue") 14]
    [else 0] ; Default case
    )
  )

(main "test1")
(main "input")
