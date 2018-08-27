;;;; Global values

(define items '())
(define item-count #f)

(define chicken-row (make-parameter 1))
(define chicken-col (make-parameter 1))
(define chicken-char #\ )

(define robot #f)

;;;;


;;;; Game sections

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
     (move-robot v: 'up))
    ((#\9)
     (move-robot v: 'up h: 'right))
    ((#\6 KEY_RIGHT)
     (move-robot h: 'right))
    ((#\3)
     (move-robot v: 'down h: 'right))
    ((#\2 KEY_DOWN)
     (move-robot v: 'down))
    ((#\1)
     (move-robot v: 'down h: 'left))
    ((#\4 KEY_LEFT)
     (move-robot h: 'left))
    ((#\7)
     (move-robot v: 'up h: 'left))
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
