(declare (unit rfc-internal))
(declare (uses rfc-const))

;;;; Herein lie internal procedures
(module rfc-internal *
  (import scheme
          (chicken base)
          (chicken format)
          (chicken random))
  (import srfi-13)
  (import ncurses)

  (import rfc-const)

  
;;;; Preliminary procedures
;;; CHICKEN 4 compatibility
  (define random pseudo-random-integer)

;;; Run the procedure f n times
  (define (repeat f n)
    (if (= n 1)
        (f)
        ((lambda ()
           (f)
           (repeat f (sub1 n))))))


;;;; Internal procedures
  (define (check-position return)
    (define (check-item-position items)
      (when (pair? items)
        (let ((item (car items)))
          ;; You bumped into an item
          (when (and (= (row item) (row robot))
                     (= (col item) (col robot)))
            (attroff A_BOLD)
            ;; Clip the message string if it's too long.
            (if (>= (string-length (message item)) (- (COLS) 3))
                (begin
                  (mvprintw 1 2 (string-take (message item) (- (COLS) 5)))
                  (addch (ACS_RARROW)) (addch (ACS_RARROW)))
                (mvprintw 1 2 (message item))) ; Print item's message
            (attron A_BOLD)
            (row-set! robot (row-prev robot))
            (col-set! robot (col-prev robot))
            (return 'bump)))
        (check-item-position (cdr items)))) ; Or was it another item?

    ;; Is it the chicken?
    (if (and (= (col chicken) (col robot))
             (= (row chicken) (row robot)))
        ;; Way to go!
        (return 'cluck))

    ;; Was it an item?
    ;; Let's inefficiently check all of them.
    (check-item-position items)

    ;; Nope, you're clear.
    (return 'move))

  (define (quit-game message code)
    ;; Stop input to avoid cluttering the terminal.
    (flushinp)
    (attroff A_BOLD)
    (clear)
    (endwin)
    (unless (zero? code)
      (fprintf (current-error-port) message)
      (newline)
      ;; Quit with an error.
      (exit code))
    (print message)
    (exit))

  (define (layout-ref key)
    (alist-ref key layout))

  ;; TODO: Is there a simpler way to do this?
  (define (switch-layout new-layout)
    (set! layout
      (case new-layout
        ((qwerty azerty qwertz)
         `((up-char    . #\k)
           (down-char  . #\j)
           (left-char  . #\h)
           (right-char . #\l)
           (up-left-char . ,(if (eqv? layout 'qwertz) #\z #\y))
           (up-right-char   . #\u)
           (down-left-char  . #\b)
           (down-right-char . #\n)))
        ((dvorak svorak)
         '((up-char    . #\t)
           (down-char  . #\h)
           (left-char  . #\d)
           (right-char . #\n)
           (up-left-char    . #\f)
           (up-right-char   . #\g)
           (down-left-char  . #\x)
           (down-right-char . #\b)))
        ((colemak)
         '((up-char    . #\e)
           (down-char  . #\n)
           (left-char  . #\h)
           (right-char . #\i)
           (up-left-char    . #\j)
           (up-right-char   . #\l)
           (down-left-char  . #\b)
           (down-right-char . #\k)))
        ((workman)
         '((up-char    . #\e)
           (down-char  . #\n)
           (left-char  . #\y)
           (right-char . #\o)
           (up-left-char    . #\j)
           (up-right-char   . #\f)
           (down-left-char  . #\v)
           (down-right-char . #\k)))
        ((f)
         '((up-char    . #\m)
           (down-char  . #\k)
           (left-char  . #\t)
           (right-char . #\l)
           (up-left-char    . #\d)
           (up-right-char   . #\r)
           (down-left-char  . #\ç)
           (down-right-char . #\z)))
        ((numpad)
         layout)
        (else
         (quit-game "Invalid layout name.\n" 1)))))

;;; Get random coordinates
  (define (random-row)
    (+ 3 (random (- (LINES) 5))))
  (define (random-col)
    (+ 1 (random (- (COLS) 2))))
  (define (random-elem lst)
    (list-ref lst (random (length lst))))

;;; Generate a number of locations to place non-robot non-chicken items
  (define (generate-items count)
    (if (not (zero? (sub1 count)))
        (cons (spawn-item
               (random-row) (random-col) 0 0
               #f (random-elem messages)
               (add1 (random 7))        ; Random colour
               (random-elem chars))
              (generate-items (sub1 count)))
        '())))
