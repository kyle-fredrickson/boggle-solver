#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORD TREE CONSTRUCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct word-tree(root branches))
;; a word-tree is a (make-word-tree root (list-of word-tree)), where root is a char, branches is a list of word trees
;; OR (make-word-tree #\nul empty)

;; build-word-tree: ... -> word-tree
;; Produces a word-tree from DICTIONARY-IN-PORT
(define (build-word-trees [trees empty] [in-port DICTIONARY-IN-PORT])
  (local [(define word (read-line in-port))]
    (cond[(eof-object? word) trees]
         [else (build-word-trees (add-word trees (string->list word)) in-port)])))

;; add-word: (listof word-tree) (listof char) -> (listof word-tree)
;; Adds a word onto the existing trees
(define (add-word trees loc)
  (cond[(empty? loc) (cons (make-word-tree #\nul empty) trees)]
       [else (local [(define tree (get-root trees (first loc)))]
               (cond[(null? tree) (cons (make-word-tree (first loc) (add-word empty (rest loc))) trees)]
                    [else (replace trees tree (make-word-tree (word-tree-root tree) (add-word (word-tree-branches tree) (rest loc))))]))]))

;; get-root: (listof word-tree) char -> word-tree
;; Returns the word tree whose root is the target or returns the trivial word tree
(define (get-root trees target)
  (cond[(empty? trees) null]
       [else (cond[(char=? (word-tree-root (first trees)) target) (first trees)]
                  [else (get-root (rest trees) target)])]))

;;get-branches: (listof word-tree) char -> (listof word-tree)
;; Returns the branches of the tree whose root is the target or returns empty
(define (get-branches trees target)
  (local [(define tree (get-root trees target))]
    (cond[(null? tree) empty]
         [else (word-tree-branches tree)])))


;; replace: (listof word-trees) word-tree word-tree -> list
;; replaces all occurences of the word-tree with the same root as old with new
(define (replace trees old new)
  (cond[(empty? trees) empty]
       [else (cond[(char=? (word-tree-root (first trees)) (word-tree-root old)) (cons new (replace (rest trees) old new))]
                  [else (cons (first trees) (replace (rest trees) old new))])]))

;; get-suffixes: (listof word-tree) (listof char) -> (listof word-tree)
;; Returns the branches from  traversing the tree along the word
(define (get-suffixes trees word)
  (cond[(empty? word) empty]
       [else (local [(define root (get-root trees (first word)))]
               (cond[(or (null? root)
                         (empty? (word-tree-branches root))) empty]
                    [(empty? (rest word)) (word-tree-branches root)]
                    [else (get-suffixes (word-tree-branches root) (rest word))]))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOGGLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a board is a MxN (listof (listof chars))
 
;; boggle-solver: board (listof word-tree) -> (listof ...)
;; Produces a list of all valid words found in a board
(define (boggle-solver board [trees TREES])
  (sort
   (remove-duplicates
    (filter (lambda (x) (>= (string-length x) 3))
            (map (lambda (x) (list->string (pairs->word board x)))
                 (finds-words (get-legal-pairs board) empty board trees)))) (lambda (x y) (string<? x y))))

;; get-legal-pairs: (listof (listof any)) -> (listof pair)
;; Returns all legal pairs in a 2d list
(define (get-legal-pairs board)
  (local [(define r (length board))
          (define c (length (first board)))
          (define l (build-list (* r c) values))]
    (map (lambda (x) (cons (quotient x c) (modulo x c))) l)))

;; find-words: (listof pair) (listof pair) board (listof word-tree) -> (listof (listof pair))
(define (finds-words next prev board trees)
  (foldr append empty
         (map (lambda (x) (find-words x (append prev (list x)) board (get-branches trees (get-letter board x)))) ;
              next))) 

;; find-words: pair (listof pair) board (listof word-tree) -> (listof (listof pair))
;; Produces a list of words from all valid moves following the current location
(define (find-words loc prev board trees)
  (local [(define next (get-next loc prev board trees))]
    (cond[(empty? next) (if (contains? char=? (map word-tree-root trees) #\nul)
                            (list prev)
                            empty)]
         [else (if (contains? char=? (map word-tree-root trees) #\nul)
                            (cons prev (finds-words next prev board trees))
                            (finds-words next prev board trees))])))

;; get-next: pair (listof pair) board (listof word-tree) -> (listof pair)
;; Produces a list of legal and worthwhile next moves
(define (get-next loc prev board trees)
  (local [(define adj (filter (lambda (x) (not (contains? pair-equal? prev x))) (get-adjacent board loc)))]
    (filter (lambda (x) (contains? char=? (map word-tree-root trees) (get-letter board x))) adj)))
    
;; contains?: (listof pair) pair -> boolean
;; Returns true if an equal pair exists in lop
(define (contains? pred lop p)
  (ormap (lambda (x) (pred x p)) lop))

;; equal?: pair pair -> boolean
;; True if values are the same
(define (pair-equal? a b)
  (and (= (car a) (car b))
       (= (cdr a) (cdr b))))

;; get-adjacent: pair board -> (listof pair)
;; Returns all legal pairs adjacent to the loc
(define (get-adjacent board loc)
  (filter (lambda (x) (legal? board x))
          (list (cons (- (car loc) 1) (- (cdr loc) 1))
                (cons (- (car loc) 1) (cdr loc))
                (cons (- (car loc) 1) (+ (cdr loc) 1))
                (cons (car loc) (- (cdr loc) 1))
                (cons (car loc) (+ (cdr loc) 1))
                (cons (+ (car loc) 1) (- (cdr loc) 1))
                (cons (+ (car loc) 1) (cdr loc))
                (cons (+ (car loc) 1) (+ (cdr loc) 1)))))

;; legal?: (listof (listof any)) (listof pair) -> boolean
;; Legal checks to see if a pair is within a board (MxN)
(define (legal? board pair)
  (and (and (>= (car pair) 0)
            (< (car pair) (length board)))
       (and (>= (cdr pair) 0)
            (< (cdr pair) (length (first board))))))

;; pairs->word: (listof (listof char)) (listof pair) -> (listof char)
;; Transforms a list of pairs into a list of the chars associated with each pair in a board
(define (pairs->word board lop)
  (map (lambda (x) (get-letter board x)) lop))

;; get-letter: board pair -> any
;; Returns the element at (x,y) in the board
(define (get-letter board loc)
  (list-ref (list-ref board (car loc)) (cdr loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLS MADE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DICTIONARY-IN-PORT (open-input-file "resources/dictionary.txt"))
(define TREES (build-word-trees))

(define b-board '((#\g #\e #\s #\n)
                  (#\a #\p #\a #\t)
                  (#\c #\h #\s #\h)
                  (#\w #\a #\w #\o)))

(define s-board '((#\c #\a)
                  (#\r #\t)))

(define list-of-words (boggle-solver b-board))
list-of-words
