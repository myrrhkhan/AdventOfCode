#lang racket
(require racket/string)
(require racket/file)

;; Purpose: solve
;; filename -> int
(define (main filename)
    (lines->solution (file->lines filename)))

;; Purpose: read lines from file and get solution
(define (lines->solution lines (running_total 0))
    (cond
        [(empty? lines) running_total]
        [else
            (let*
                    ;; take first line, remove "Game x: " with split
                    ((line (second (string-split (first lines) ": "))))
                    ;; get linepower from first line, add that linepower to linepower of rest of lines through recursion
                    (lines->solution (rest lines) (+ running_total (line->linepower line)))
            )
        ]
    )
)

(define (line->linepower line)
    ;; make local loop for tail recursion
    ;; replace ; with , in line, then split by ", " into substrings
    (let loop ((substrings (string-split (string-replace line ";" ",") ", "))
                (maxred 0)
                (maxblue 0)
                (maxgreen 0)) 
        (cond
            [(empty? substrings) (* maxred maxblue maxgreen)]
            [else
                (let* ((numcolor (string-split (first substrings) " "))
                        (num (string->number (first numcolor)))
                        (color (second numcolor)))
                    (cond
                        [(equal? color "red")
                            (loop (rest substrings) (max num maxred) maxblue maxgreen)]
                        [(equal? color "blue")
                            (loop (rest substrings) maxred (max num maxblue) maxgreen)]
                        [(equal? color "green")
                            (loop (rest substrings) maxred maxblue (max num maxgreen))]
                    )
                )
            ]
        )
    )
)

(main "test1")
(main "input")
