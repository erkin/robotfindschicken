#!/usr/bin/csi -s
(use ncurses)
(require-extension srfi-13)
(require-extension srfi-18)

;;;; Preliminary procedures

(define (repeat f n) ; run the procedure f n times
  (if (= n 1)
      (f)
      ((lambda ()
         (f)
         (repeat f (sub1 n))))))

;;;;


;;;; Constant values and global variables

;;; Colours
(define-constant *frame-colour*   1)
(define-constant *decor-colour*   2)
(define-constant *chicken-colour* 3)
(define-constant *message-colour* 4)
(define-constant *help-colour*    5)
(define-constant *control-colour* 6)
(define-constant *extra-colour*   7)
(define-constant *robot-colour*   8)

(define-constant messages
  '("A bottle of a soft drink. Warm and flat."
    "An action figure of Guy Steele, 1:18 scale."
    "This is a VAXServer running OpenVMS 8.3."
    "A potted plant of dubious legality. It needs watering."
    "A picture of a woman wearing a hat with feathers. Compression artifacts distorted her face but it looks familiar."
    "A 3D model of a bunny containing 69451 polygons. You do not feel like verifying that claim."
    "This is a Canterbury corpus in folio, bound in leather of Stanford bunnies."
    "This is almost a chicken, but not quite. Try harder."
    "A cheque from Donald Knuth, for one hexadecimal dollar."
    "This is a teapot. You are certain that you've seen it somewhere."
    "A semi-automatic, conservatory key system oboe."
    "A well-worn set of Slackware 1.0.0 installation floppy set."
    "An EBCDIC-art picture of a naked woman printed with a line printer."
    "An ANSI-art picture of a naked man printed with a dot-matrix printer."
    "An ASCII-art picture of a naked person printed with a daisy-wheel printer."
    "One of Paul Graham's oil paintings."
    "An empty set. It's barely visible."
    "A LART made from depleted uranium."
    "There is absolutely nothing here.                       (fnord?)"
    "You have no idea what this it, but you hope it's not what it looks like."
    "This is a cheap knock-off of the platinum-iridium kilogramme standard."
    "These are two bodies connected by a weightless string."
    "A gift-wrapped set of straightedge and compasses."
    "A handheld electronic console 'Game of Life'. Glidey!"
    "A finite state machine. It appears to have stopped. Problematic."
    "A fork that once belonged to one of the dining philosophers."
    "This is a sleeping barber. Don't touch his locks."
    "A +0 uncursed cockatrice corpse."
    "A defiled altar formerly dedicated to the god of IT security."
    "An electric cdr."
    "It's a sleeping daemon."
    "An old keyboard. The hyper key seems to be missing."))

(define-constant chars
  '(#\! #\@ #\# #\$ #\% #\& #\*
    #\/ #\= #\? #\+ #\- #\_ #\\
    #\| #\' #\, #\. #\; #\: #\"
    #\< #\> #\` #\~))

(define-record item
  row col message colour char)

(define-record robot
  row col row-prev col-prev
  moved? colour char)

(define items '())

(define item-count #f)

;; Parameters to be altered
(define chicken-row (make-parameter 1))
(define chicken-col (make-parameter 1))

(define chicken-char #\ )

;;;;


;;;; Internal procedures

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
      (cons (make-item
             (random-row) (random-col)
             (random-elem messages)
             (add1 (random 7)) ; Random colour
             (random-elem chars))
            (generate-items (sub1 count)))
      '()))

;;; Write messages in the middle of the window
(define (centre-message . messages)
  (define (centre-message-iter messages n) ; with doublespacing
    (when (pair? messages)
          (mvprintw (+ (quotient (LINES) 2) (- n (length messages)))
                    (quotient (- (COLS) (string-length (car messages))) 2)
                    (car messages))
          (centre-message-iter (cdr messages) (add1 n))))
  (centre-message-iter messages 0))

;;;;

(define robot #f)

;;;; Drawing procedures

;;; Draw non-robot non-chicken items
(define (draw-items item-list)
  (unless (null? item-list)
          (let ((item (car item-list)))
            (attron (COLOR_PAIR (item-colour item)))
            (mvaddch (item-row item) (item-col item)
                     (item-char item))
            (attroff (COLOR_PAIR (item-colour item))))
          (draw-items (cdr item-list))))

(define (draw-chicken)
  (attron (COLOR_PAIR *chicken-colour*))
  (mvaddch (chicken-row) (chicken-col) chicken-char)
  (attroff (COLOR_PAIR *chicken-colour*)))

(define (draw-robot)
  ;; Remove the previous robot
  (mvaddch (robot-row-prev robot) (robot-col-prev robot) #\ )

  (attron (COLOR_PAIR (robot-colour robot)))
  (mvaddch (robot-row robot) (robot-col robot) (robot-char robot))
  (attroff (COLOR_PAIR (robot-colour robot)))

  ;; Reset status
  (robot-moved?-set! robot #f))

(define (draw-frame)
  ;; Frame around the box
  (attron (COLOR_PAIR *frame-colour*))
  (box (stdscr) (ACS_VLINE) (ACS_HLINE))
  (mvhline 2 1 (ACS_HLINE) (- (COLS) 2))
  (mvaddch 2 0 (ACS_LTEE))
  (mvaddch 2 (sub1 (COLS)) (ACS_RTEE))
  (attroff (COLOR_PAIR *frame-colour*))

  ;; Fancy branding
  (attron (COLOR_PAIR *decor-colour*))
  (mvprintw 2 2 "robotfinds")
  (attroff (COLOR_PAIR *decor-colour*))

  (attron (COLOR_PAIR *chicken-colour*))
  (printw "chicken")
  (attroff (COLOR_PAIR *chicken-colour*)))

;;;;


;;;; Movement procedures

(define (check-position return)
  (define (check-item-position items)
    (when (pair? items)
          (let ((item (car items))) ; You bumped into an item
            (when (and (= (item-row item) (robot-row robot))
                       (= (item-col item) (robot-col robot)))
                  ;; Clip the string if it's too long.
                  (if (>= (string-length (item-message item)) (- (COLS) 3))
                      (begin
                        (mvprintw 1 2 (string-take (item-message item) (- (COLS) 5)))
                        (addch (ACS_RARROW))
                        (addch (ACS_RARROW)))
                      (mvprintw 1 2 (item-message item))) ; Print item's message
                  (robot-row-set! robot (robot-row-prev robot))
                  (robot-col-set! robot (robot-col-prev robot))
                  (return #f)))
          (check-item-position (cdr items)))) ; Or was it another item?

  (if (and (= (chicken-col) (robot-col robot)) ; You found chicken!
           (= (chicken-row) (robot-row    robot)))
      (rfc-win))

  (check-item-position items)
  (return #t)) ; Nope, you're clear.


(define (move-robot #!key (v 'nil) (h 'nil))
  ;; Erase the message line
  (mvprintw 1 2 (make-string (- (COLS) 3) #\ ))

  ;; Save old position
  (robot-row-prev-set! robot (robot-row robot))
  (robot-col-prev-set! robot (robot-col robot))

  ;; Vertical movement
  (if (eq? v 'up)
      (robot-row-set! robot (sub1 (robot-row robot)))
      (if (eq? v 'down)
          (robot-row-set! robot (add1 (robot-row robot)))))

  ;; Horizontal movement
  (if (eq? h 'left)
      (robot-col-set! robot (sub1 (robot-col robot)))
      (if (eq? h 'right)
          (robot-col-set! robot (add1 (robot-col robot)))))

  ;; Vertical boundary check
  (if (= (robot-row robot) (- (LINES) 1))
      (robot-row-set! robot (- (LINES) 2))
      (if (= (robot-row robot) 2)
          (robot-row-set! robot 3)))

  ;; Horizontal boundary check
  (if (= (robot-col robot) (- (COLS) 1))
      (robot-col-set! robot (- (COLS) 2))
      (if (= (robot-col robot) 0)
          (robot-col-set! robot 1)))

  ;; Did we move or bump into something?
  (robot-moved?-set! robot
                     (call-with-current-continuation
                      (lambda (return) (check-position return)))))

;;;;


;;;; Game sections

(define (quit-game message code)
  ;; Stop input to avoid cluttering the terminal
  (flushinp)
  (attroff A_BOLD)
  (clear)
  (endwin)
  (unless (zero? code)
          (fprintf (current-error-port) message)
          (exit code)) ; quit with an error
  (print message)
  (exit))

(define (rfc-splash)
  (draw-frame)
  ;; The ordering is important to avoid overwriting
  ;; some parts with whitespace.

  (attron (COLOR_PAIR *help-colour*))
  (mvprintw (- (LINES) 2) (- (COLS) 18) "Press Q to exit.")
  (mvprintw (- (LINES) 3) 2 "Use       to move.")
  (mvprintw 3 3 "Press any key to begin/resume.")
  (attroff (COLOR_PAIR *help-colour*))
  
  (attron (COLOR_PAIR *control-colour*))
  (mvprintw (- (LINES) 4) 6 "7 8 9")
  (mvprintw (- (LINES) 3) 6 "4   6")
  (mvprintw (- (LINES) 2) 6 "1 2 3")
  (attroff (COLOR_PAIR *control-colour*))

  (attron (COLOR_PAIR *message-colour*))
  (mvaddch (- (LINES) 3) 8 (ACS_PLUS))
  (attroff (COLOR_PAIR *message-colour*))

  ;; Mock the player for exiting on help screen.
  (if (member (getch) '(#\q #\Q KEY_F0))
      (quit-game "That was quick." 0))

  ;; Now that the player pressed a key, clear the screen.
  (clear) 

  ;; And redraw the game elements.
  (draw-frame)
  (draw-items items) ; how descriptive
  (draw-robot)
  (draw-chicken))

(define (rfc-init)
  (initscr)

  (if (not (has_colors))
      (quit-game "Your terminal does not support colours." 1))

  (if (or (< (COLS) 32)
          (< (LINES) 16))
      (quit-game "Your terminal is too small to run this." 1))
  
  ;; Give integer aliases to colours. (See constants above.)
  (start_color)
  (init_pair *frame-colour* COLOR_BLUE COLOR_BLACK)
  (init_pair *decor-colour* COLOR_CYAN COLOR_BLACK)
  (init_pair *chicken-colour* COLOR_RED COLOR_BLACK)
  (init_pair *message-colour* COLOR_YELLOW COLOR_BLACK)
  (init_pair *help-colour* COLOR_GREEN COLOR_BLACK)
  (init_pair *control-colour* COLOR_MAGENTA COLOR_BLACK)
  (init_pair *extra-colour* COLOR_BLUE COLOR_BLACK)
  (init_pair *robot-colour* COLOR_GREEN COLOR_BLUE)

  (set! items (generate-items
               (if item-count
                   (if (< item-count (COLS))
                       ;; User provided amount of items
                       item-count
                       ;; Set it to number of columns if it's too many.
                       (COLS))
                   ;; Default is (columns+lines)/10
                   (quotient (+ (COLS) (LINES)) 10))))
  (set! chicken-char (random-elem chars))

  (raw) (noecho)
  (keypad (stdscr) #t)
  (curs_set 0)

  (attron A_BOLD)

  (set! robot
        (make-robot 
         (random-row) (random-col) 5 5
         #f *robot-colour* (ACS_DIAMOND)))

  (chicken-row (random-row))
  (chicken-col (random-col))

  (attron (COLOR_PAIR *message-colour*))
  (mvprintw 1 (- (COLS) 32) "Press H any time to view help.")
  (centre-message "You are the Robot!" "Find the Chicken!" "Godspeed!")
  (attroff (COLOR_PAIR *message-colour*))

  (rfc-splash)
  (rfc-loop))

(define (rfc-loop)
  ;; Only redraw if the action taken was movement.
  ;; Other keys don't result in a redraw.
  (if (robot-moved? robot) (draw-robot))

  (case (getch)
    ((#\q #\Q KEY_F0)
     (quit-game "You couldn't find the Chicken. Sad!" 0))
    ((#\8 KEY_UP)
     (move-robot
      v: 'up))
    ((#\9)
     (move-robot
      v: 'up
      h: 'right))
    ((#\6 KEY_RIGHT)
     (move-robot
      h: 'right))
    ((#\3)
     (move-robot
      v: 'down
      h: 'right))
    ((#\2 KEY_DOWN)
     (move-robot
      v: 'down))
    ((#\1)
     (move-robot
      v: 'down
      h: 'left))
    ((#\4 KEY_LEFT)
     (move-robot
      h: 'left))
    ((#\7)
     (move-robot
      v: 'up
      h: 'left))
    ((#\h #\H KEY_F1 KEY_HELP)
     (rfc-splash)))

  (rfc-loop))

(define (rfc-win)
  ;; Clear the screen but redraw the frame.
  (clear)
  (flushinp) ; stop taking input
  (draw-frame)

  (mvprintw 1 (- (COLS) 9) "Aww...")

  ;; Place the robot on the message line.
  (robot-row-set! robot 1)
  (robot-col-set! robot (+ (quotient (COLS) 2) 4))

  ;; And also the chicken.
  (chicken-row 1)
  (chicken-col (- (quotient (COLS) 2) 5))

  (draw-robot)
  (draw-chicken)
  (draw-items items) ; let the items remain

  ;; Step by step / Move a little closer to me
  (repeat
   (lambda _
     (thread-sleep! 0.8)
     (move-robot h: 'left)
     (chicken-col (add1 (chicken-col)))
     (draw-robot)
     (draw-chicken)
     (mvaddch (chicken-row) (sub1 (chicken-col)) #\ )
     (refresh))
   4)

  (thread-sleep! 0.5)

  (attron (COLOR_PAIR *message-colour*))
  (centre-message "You found the Chicken!" "Good Robot!")
  (attroff (COLOR_PAIR *message-colour*))

  (attron (COLOR_PAIR *help-colour*))
  (mvprintw (- (LINES) 2) (- (COLS) 24) "Press R to play again.")
  (attroff (COLOR_PAIR *help-colour*))

  (refresh)
  (thread-sleep! 1)

  ;; Play again.
  (when (member (getch) '(#\r #\R))
        (clear)
        (endwin)
        (rfc-init))

  ;; Exit on any other key.
  (quit-game "Thanks for playing!" 0))

;;;;


;;;; Main

(define (rfc-usage)
  (print "Usage: rfc [-n ITEMS]")
  (print "  --help        Display this help message")
  (print "  --items ITEMS Play the game with ITEMS number of items")
  (print "  --version     Display version and licence information")
  (exit))

(define (rfc-version)
  (print   "robotfindschicken v1.-1.5")
  (print   "Copyright (C) 2018 Erkin Batu Altunbaş")
  (newline)
  (print* "Each file of this project's source code is subject ")
  (print  "to the terms of the Mozilla Public Licence v2.0")
  (print* "If a copy of the MPL was not distributed with this file, ")
  (print  "you can obtain one at https://mozilla.org/MPL/2.0/")
  (exit))

(define (rfc-options args)
  (define (rfc-items rest)
    (if (null? rest)
        (rfc-usage))
    (let ((n (string->number (car rest))))
      (cond
       ((not n)
        (print "Not a valid number: " (car rest))
        (newline)
        (exit 1))
       ((and
         (exact? n)
         (> n 0))
        (set! item-count n)
        (rfc-init))
       (else
        (print "Not a suitable number: " n)
        (print "Ignoring...")
        (newline)
        (rfc-init)))))

  (let ((arg (car args)))
    (cond
     ((member arg '("-h" "-H" "-?" "--help" "-help"))
      (rfc-usage))
     ((member arg '("-v" "-V" "--version" "-version"))
      (rfc-version))
     ((member arg '("-n" "-N" "--items" "-items"))
      (rfc-items (cdr args)))
     (else
      (rfc-usage)))))

(define (main args)
  (if (null? args)
      (rfc-init)
      (rfc-options args)))

(main (command-line-arguments))

;;;;
