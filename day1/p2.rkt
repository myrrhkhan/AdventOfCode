#! /opt/homebrew/Caskroom/racket
#lang racket/base
(require racket/file)
(require racket/list)
(require expect/rackunit)
(require racket/bool)
(require racket/string)

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(require readline)

(define (main filepath)
    (define inputlines (file->lines filepath)) ; read file into listof strings
    (inputlines->int inputlines)
)

;; lines, int -> int
;; converts file lines sum of the desired integers on each line
(define (inputlines->int inputlines [running_total 0])
    (cond 
        [(empty? inputlines) running_total]
        [else 
             (set! running_total (+ running_total (int_from_line (first inputlines))))
             (inputlines->int (rest inputlines) running_total)
        ]
    )
)

(define (int_from_line line)
    ;; initial int values
    (define firstint -1)
    (define secondint -1)
    ;; convert spellings to numbers
    (set! line (replacenumspell line))
    ;; convert to vector. O(n) time but grabbing element and size is O(1) time 
    (set! line (list->vector (string->list line)))
    ;; loop through beginning of list, find first int
    (define idx 0)
    (while (= firstint -1)
        (set! firstint (parseint line idx))
        (set! idx (+ idx 1))
    )
    ;; loop from end of list, find last int
    (set! idx (- (vector-length line) 1))
    (while (= secondint -1)
        (set! secondint (parseint line idx))
        (set! idx (- idx 1))
    )
    (+ (* firstint 10) secondint)
)

(define (replacenumspell line)
    (set! line (string-replace line "one" "one1one"))
    (set! line (string-replace line "two" "two2two"))
    (set! line (string-replace line "three" "three3three"))
    (set! line (string-replace line "four" "four4four"))
    (set! line (string-replace line "five" "five5five"))
    (set! line (string-replace line "six" "six6six"))
    (set! line (string-replace line "seven" "seven7seven"))
    (set! line (string-replace line "eight" "eight8eight"))
    (set! line (string-replace line "nine" "nine9nine"))
    line
)

;; line: list, idx number -> idx int
;; parses int from char at idx, if parseable return val else -1
(define (parseint line idx)
    ;; take character, convert to list with 0, convert that list to a string, and from string->number
    (define int (string->number (make-string 1 (vector-ref line idx))))
    ;; determine output
    (cond [(not (false? int)) int]
          [else '-1]
    )
)

(check-expect (main "test2") 281)
(main "test2")
(main "input")
