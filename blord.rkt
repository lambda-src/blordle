#lang racket

; Escape codes for colors 
(define grey "\u001b[90m")
(define yellow "\u001b[33m")
(define green "\u001b[32m")
(define reset "\u001b[0m")

(define (print-green char)
    (display (format "~a~a~a" green char reset)))

(define (print-yellow char) 
    (display (format "~a~a~a" yellow char reset)))

(define (print-grey char)
    (display (format "~a~a~a" grey char reset)))

; Gets a random word from the word bank
(define (get-word)
    (define get-lines
        (call-with-input-file "words.txt" 
            (lambda (file) 
                (for/list ([line (in-lines file)])
                    line))))
    (list-ref get-lines (random (length get-lines))))

; Get user input and make sure its 5 characters long
(define (get-input)
    (define guess (string-downcase (read-line)))
    (if (= (string-length guess) 5)
        guess
        ; Begin groups these expressions into a single expression
        (begin 
            (displayln "Your word was too long you must use 5 letters")
            (get-input))))

; Returns a list of the colors of a given word
(define (get-colors guess word)
    (for/list ([g-char (in-string guess)]
          [w-char (in-string word) ])
        (cond 
            ; The 'x are atoms which are constants who's value equals their name 
            [(char=? g-char w-char) 'green]
            [(string-contains? word (string g-char)) 'yellow]
            [else 'grey])))

; Display the guess with all the colors 
(define (display-guess guess guess-colors)
    (for ([g-char (in-string guess)]
          [color guess-colors])
        (cond 
            [(eq? color 'green) (print-green g-char)]
            [(eq? color 'yellow) (print-yellow g-char)]
            [else (print-grey g-char)]))
    (newline))

(define (game-loop word guess-count)
    (define guess (get-input))
    (define colors (get-colors guess word))
    ; Check if every color is green
    (define (won? colors)
        (andmap (lambda (color) (eq? color 'green)) colors))
    (display-guess guess colors)
    (if (won? colors) 
        (displayln (format "You got '~a' in ~a guess" word guess-count))
        (if (< guess-count 6)
            (game-loop word (+ guess-count 1))
            (begin 
                (displayln (format "You lost the word was: '~a'" word))
                (newline)))))

(displayln "Welcome to blordle!\nEnter a 5 letter word, you only have 6 guesses!")
(game-loop (get-word) 0)