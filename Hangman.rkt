(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-words-from filename)
  (let* ((port (open-input-file filename))
         (res (read-word-list port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-word-list port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-word-list port
                        (cons (symbol->string stuff) acc)))))

(define list-of-words (read-words-from source-name))

;; STATE OF THE GAME
(define word-to-guess null)
(define partial-sol null)

(define hits 0)
(define plays 0)
(define failures 0)
(define total-failures 6)
(define total-hits (length word-to-guess))
(define glossary (map string->list list-of-words))
 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 (< failures total-failures))
                ""
                (string-append "GAME-OVER(" (list->string word-to-guess) ")"))
    )
  )
)

;; F1
;; Occurrences
;; This function is used to count how many times chars occur in word.
(define (occurrences word char)
  (cond
    [(empty? word) 0]
    [(equal? (car word) char) (+ (occurrences (cdr word) char) 1)]
    [else (occurrences (cdr word) char)]
   )
 )

;(occurrences '(#\a #\b #\b) #\b)
;(occurrences '() #\b)

;; Indices
;; This function is used for returning a list that starts from
;; 0 to n - 1 which will be use in the indices functiom to return
;; the correponding to position of char in the words.
(define (lst-indices n)
  (define (r n)
    (cond
      ((equal? n 0) '())
      (else (cons (- n 1) (r (- n 1)))))
    )
    (reverse (r n))
)

;; This function is for returning the list of indices to each
;; position and char will be in the list so it is index
;; then reverse to the correct order.
(define (indices word char)
  (define (acc-index-char cur-char index acc) ; This where the cur-char is given by the list of words
    (cond                                     ; so it can be indexed
      ((equal? cur-char char) (cons index acc))
      (else acc)))
  (reverse (foldl acc-index-char '() word (lst-indices (length word))))
)

;(indices '() #\b)
;(indices '(#\a #\b #\b) #\b)

;; Number of Hits
;; This function is for counting the number of hits of hidden words.
(define (noOfHits hidden)
  (cond
    [(empty? hidden) 0]
    [else (if (not (equal? (car hidden) #\*))
        (+ 1 (noOfHits (cdr hidden)))
        (+ 0 (noOfHits (cdr hidden))))]
   )
)

;(noOfHits '(#\a #\* #\*))
;(noOfHits '(#\* #\* #\*))

;; Replace Indices
;; This function is for replacing each position given by indices function
;; with the index of the word to new then will return the list of the indexed
;; result in listing . 
(define (replace-indices word idx new)
  (map (lambda (char index) ; This is for checking if the index is in idx so
         (cond              ; that postion of the list will be replace by new.
            ((member index idx) new)
            (else char)))
       word (lst-indices (length word)) ; This is where is used the function lst-indices 
  )                                     ; for the length.
)

;(replace-indices '(#\a #\* #\*) '(1 2) #\b)
;(replace-indices '(#\a #\* #\*) '() #\b)

;; F2
;; Side effects
;; IO(String)

;; Restart
;; This function is actually for playing the hangman game
;; in where I set up the rules and functions such as a function to
;; generate a random word to guees from glossary.txt file and also
;; a fuction for partial guess which will show where are guess
;; word location are and then I have bigining sequence in where the
;; rules are set.
(define (restart)
  (define (generate-random-word) ; This is for getting any random word to guess from glossary.
    (list-ref glossary (random (length glossary)))
  )

  (define (partial-guess word) ; This is for resting the partially guessed list of *. 
    (map (lambda (x) #\*) (lst-indices (length word)))
  )

  (begin ; This is the actually rules of the game.
    (set! word-to-guess (generate-random-word))
    (set! partial-sol (partial-guess word-to-guess))
    (set! plays 0)
    (set! hits 0)
    (set! total-hits (length word-to-guess))
    (set! failures 0)
    (set! total-failures 6)
    (game-status)
  )
)

;(restart)

;; Char -> IO(String)
;; Guess
;; This function is for the guessing functionallity (guess #\"a character")in where
;; game rules are the called to check if the is the game is over so it can check every
;; character from word-to-guess if those in the condition it will increase the plays
;; and then it will rutrun the game status
(define (guess char)
  (cond
    ((not (or (>= failures total-failures) (>= hits total-hits)))

     (define lst-indices-char (indices word-to-guess char))
     (begin
       (set! partial-sol (replace-indices partial-sol lst-indices-char char)) ; Upadates partially guessed works. 
       (set! hits (noOfHits partial-sol)) ; This will compute the hits by given char.
       (set! plays (+ plays 1)) ; This will increase number plays by 1.
       (cond
         ((empty? lst-indices-char)
          (begin
            (set! failures (+ failures 1)))))) ; This increases failures.
      )
   )
  (game-status) ; This will return the game status and the end.
)

;(guess #\a)

;; IO(String)
;; Solve
;; This function will guess all the characters in word then it will
;; return the game status.
(define (solve word)
  (begin
    (map guess (string->list word))
    (game-status)
   )
)

;(solve "calamites")

;; EXTRA F3
;; p: all-words as list of list of char
;; Words Containing
;; The function is for showing the results of words = all-words that have the characters
;; as list characters.
(define (words-containing words char)
  (define (no-occurrences word)
    (< 0 (occurrences word char))
  )
  (filter no-occurrences words)
)

;(words-containing '((#\b #\u #\s)
;                    (#\b #\a #\r)   
;                    (#\c #\a #\r))
;                  #\b)

;(words-containing '((#\b #\u #\s)
;                    (#\b #\a #\r)   
;                    (#\c #\a #\r))
;                  #\c)

;; p: all-words as list of list of char
;;  : chars as a list of char
;; Words Contaning Ext
;; This function is for showing the words character in as a list of characters.
(define (words-containing-ext words chars)
  (foldl (lambda (char acc) ; This will take the filtered list of words from the last lambda call.
           (words-containing acc char)) words chars) ; This will only get the
)                                                    ; characters of the current ones

;(words-containing-ext '((#\b #\u #\s)
;                        (#\b #\a #\r)   
;                        (#\c #\a #\r))
;                      '(#\a))

;(words-containing-ext '((#\b #\u #\s)
;                        (#\b #\a #\r)   
;                        (#\c #\a #\r))
;                      '(#\a #\b))

;; IO([String])
;; Sieve
;; This function will just return all the strings from the from glossary which
;; have the characters.
(define (sieve chars)
  (map list->string (words-containing-ext glossary chars))
)

;(sieve '(#\a #\e #\h))
