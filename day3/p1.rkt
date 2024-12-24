(define (main filename)
    (findsums (file->lines filename)))

;; global variables
;; Symbols and nums to check at current iteration (from top line):
(define symbolhash_top (make-hash empty))
(define numstocheck_top (make-hash empty))
;; Symbols and nums to check at next iteration (from current line):
(define symbolhash_curr (make-hash empty))
(define numstocheck_curr (make-hash empty))
;; Statuses
(define numsearch true)
(define symbolsearch true)
;; Running total
(define running_total 0)


;; Main function to find the sum of all the eligible numbers by editing the running_total var
(define (findsums lines (symbolhash_top (make-hash empty)) (numstocheck_top (make-hash empty)))
    (define line (first lines))
    ;; parse line, update running_total and curr hashmaps, 
    ;; get last index of line to set upper bound of loop
    (define lastidx (lineiterate line)) 
    ;; TODO iterate through hashmaps
    ;; Check numstocheck_curr
    (recheck lasidx)
    )

;; Iterate through individual lines
;; (listof string int listof int listof int)
;; - line as listof string, recursively goes through line
;; - index of line
;; - if there previous char(s) were numbers, keep track of them with current_nums and idxlist
(define (lineiterate line (idx 0) (current_nums empty) (idxlist empty))
    (if (empty? line) idx)
    (define char (first line))
    (cond
        ;; Compare character
        [(eq? char '.')
            (cond 
                ;; if there was a symbol before the character (i.e. if numsearch was true),
                ;; add current_nums to running total, set numsearch to false, reset num and idx lists, iterate
                [numsearch
                    (set! running_total (+ running_total (foldl + current_nums))) ;; add sum
                    (set! numsearch #f)
                    (lineiterate (rest line) (+ idx 1)) ;; recurse through rest of line
                    ]
                ;; else, either there was a number w/o a symbol preceding the period, or no nums/symbols
                ;; if nothing, business as usual, else, add current_nums to numstocheck_curr
                ;; sum_update_prevnums handles both of these cases
                [else (sum_update_prevnums current_nums idxlist)]
                )
            ]
        ;; if character is a number, look for more numbers, turn symbolsearch on
        [(string->number char)
            (define num (string->number char))
            (set! symbolsearch #t)
            (lineiterate (rest line) (+ 1 idx) (current_nums (cons num)) (idxlist (cons idx)))
            ]
        ;; if symbol, add previous numbers, add symbol to hash above, turn numsearch on
        [else
            (sum_update_prevnums current_nums idxlist)
            (set! numsearch #t)
            (lineiterate (rest line) (+ 1 idx))
            ]
        )
    )

;; Handles cases where
;; 1. symbol was found
;; 2. number was found without any symbols in front or behind it
;; 3. no number ("base" case)
(define (sum_update_prevnums current_nums idxlist)
    (set! symbolsearch #f)
    ;; case 3: return 0, no change
    (if (empty? current_nums) 0)
    (define sum (foldl + current_nums))
    (cond
        ;; case 1: symbol was found with prior numbers
        ;; add sum to running_total
        [numsearch 
            (set! running_total (+ sum running_total)) 
            ]
        ;; case 2: if no symbol, add to numstocheck_curr to check later
        [else 
            ;; idxlist should have all indeces that border the number
            (set! idxlist (idxlist (cons (- 1 (first idxlist) (cons (+ 1 (last idxlist)))))))
            (update_numstocheck sum idxlist)
            ]
        )
    )


;; Purpose: take a number, possible idxs with symbols, and add to numstocheck_curr
;; int listof int -> void
(define (update_numstocheck sum idxlist)
    ;; TODO: (update-hash numstocheck_curr (first idxlist) sum)
    (update_numstocheck sum (rest idxlist))
    )

;; Purpose: check current nums to see if there are any symbols on top, and then check top numbers
(define (recheck bound)
    ;; iterate through numbers up to bound
    ;; check numstocheck_curr, see if there's a number to check
    ;; if there is a number to check, check symbolhash_top and see if there's a symbol there
    ;; if so, add to running total, delete next entries by checking difference
    ;; now, iterate through indeces again
    ;; if symbolhash_curr has value, check value in numstocheck_top, add any value that's there
    ;; set symbolhash_top to symbolhash_curr, set numstocheck_top to numstocheck_curr
    ;; set symbolhash_curr and numstocheck_curr to empty
    )
