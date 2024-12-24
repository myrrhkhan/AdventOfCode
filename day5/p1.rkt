;; Pseudocode:
;; Read file into function
;; - first line, split by ":", save numbers
;; readmap: take seeds as input
;; - macros! make macro, quote label before ":" as function if function has ":"
;; - pass list of list of threes, for, if none, return just that value, continue iterating until blank line
;;  - how? function to build list thingy
;; - then analyze the seeds using that function

;; main, takes filename, analyzes seeds and map, returns
(define (main filename)
    (define lines (file->lines filename)
    (define seeds (first lines))
)

(define (readmap lines)
  )

