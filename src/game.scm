;;;; Herein lie game mechanics

(module rfc-game *
  (import scheme
          (chicken base))
  (import srfi-18)
  (import ncurses)

  (import rfc-const
          rfc-internal
          rfc-draw)


;;;; Movement procedures

  (define (move-robot #!key (v 'nil) (h 'nil))
    ;; Erase the message line
    (mvprintw 1 2 (make-string (- (COLS) 3) #\ ))

    ;; Save old position
    (row-prev-set! robot (row robot))
    (col-prev-set! robot (col robot))

    ;; Vertical movement
    (if (eq? v 'up)
        (row-set! robot (sub1 (row robot)))
        (if (eq? v 'down)
            (row-set! robot (add1 (row robot)))))

    ;; Horizontal movement
    (if (eq? h 'left)
        (col-set! robot (sub1 (col robot)))
        (if (eq? h 'right)
            (col-set! robot (add1 (col robot)))))

    ;; Vertical boundary check
    (if (= (row robot) (- (LINES) 1))
        (row-set! robot (- (LINES) 2))
        (if (= (row robot) 2)
            (row-set! robot 3)))

    ;; Horizontal boundary check
    (if (= (col robot) (- (COLS) 1))
        (col-set! robot (- (COLS) 2))
        (if (= (col robot) 0)
            (col-set! robot 1)))

    ;; Did we move or bump into something?
    (case (call-with-current-continuation
           (lambda (return) (check-position return)))
      ((bump) (moved! robot #f))
      ((move) (moved! robot #t))
      ((cluck) (rfc-win))))


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
    (draw-layout 2 '(down-left-char down-char down-right-char) " ")
    (draw-layout 3 '(left-char right-char) "   ")
    (draw-layout 4 '(up-left-char up-char up-right-char) " ")
    (attroff (COLOR_PAIR *control-colour*))

    (attron (COLOR_PAIR *message-colour*))
    (mvaddch (- (LINES) 3) 8 (ACS_PLUS))
    (attroff (COLOR_PAIR *message-colour*))

    ;; Mock the player for exiting on help screen.
    (if (member (getch) '(#\q #\Q KEY_F0 KEY_EXIT KEY_CLOSE))
        (quit-game "That was quick." 0))

    ;; Now that the player pressed a key, clear the screen.
    (clear) 

    ;; And redraw the game elements.
    (draw-frame)
    (draw-items items)                  ; how descriptive
    (draw-robot robot)
    (draw-chicken chicken))

  (define (rfc-init layout)
    (initscr)

    (if layout
        (switch-layout layout))

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

    (raw) (noecho)
    (keypad (stdscr) #t)
    (curs_set 0)

    (attron A_BOLD)

    (set! robot
      (spawn-item
       (random-row) (random-col) 5 5
       #f "" *robot-colour* (ACS_DIAMOND)))

    (set! chicken (car (generate-items 2)))

    (attron (COLOR_PAIR *message-colour*))
    (mvprintw 1 (- (COLS) 32) "Press ? any time to view help.")
    (centre-message "You are the Robot!" "Find the Chicken!" "Godspeed!")
    (attroff (COLOR_PAIR *message-colour*))

    (rfc-splash)
    (rfc-loop))

  (define (rfc-loop)
    ;; Only redraw if the action taken was movement.
    ;; Other keys don't result in a redraw.
    (if (moved? robot) (draw-robot robot))

    ;; CHICKEN 5 removes `select` and it's too late to go back to `case`
    ;; and I'm too lazy to write a macro for it. I'm terribly sorry.
    (let ((input (getch)))
      (cond 
       ((member input (list #\q #\Q KEY_F0 KEY_EXIT KEY_CLOSE))
        (quit-game "You couldn't find the Chicken. Sad!" 0))
       ((member input (list (layout-ref 'up-char) KEY_UP))
        (move-robot v: 'up))
       ((eq? input (layout-ref 'up-right-char))
        (move-robot v: 'up h: 'right))
       ((member input (list (layout-ref 'right-char) KEY_RIGHT))
        (move-robot h: 'right))
       ((eq? input (layout-ref 'down-right-char))
        (move-robot v: 'down h: 'right))
       ((member input (list (layout-ref 'down-char) KEY_DOWN))
        (move-robot v: 'down))
       ((eq? input (layout-ref 'down-left-char))
        (move-robot v: 'down h: 'left))
       ((member input (list (layout-ref 'left-char) KEY_LEFT))
        (move-robot h: 'left))
       ((eq? input (layout-ref 'up-left-char))
        (move-robot v: 'up h: 'left))
       ((member input (list #\? KEY_HELP))
        (rfc-splash))))

    (rfc-loop))

  (define (rfc-win)
    ;; Clear the screen but redraw the frame.
    (clear)
    ;; Stop taking input
    (flushinp)
    (draw-frame)

    (mvprintw 1 (- (COLS) 9) "Aww...")

    ;; Place the robot on the message line.
    (row-set! robot 1)
    (col-set! robot (+ (quotient (COLS) 2) 4))

    ;; And also the chicken.
    (row-set! chicken 1)
    (col-set! chicken (- (quotient (COLS) 2) 5))

    (draw-robot robot)
    (draw-chicken chicken)
    (draw-items items)                  ; let the items remain

    ;; Step by step / Move a little closer to me
    (repeat
     (lambda _
       (thread-sleep! 0.8)
       (move-robot h: 'left)
       (col-set! chicken (add1 (col chicken)))
       (draw-robot robot)
       (draw-chicken chicken)
       (mvaddch (row chicken) (sub1 (col chicken)) #\ )
       (refresh))
     4)

    (thread-sleep! 0.5)

    (attron (COLOR_PAIR *message-colour*))
    (centre-message "You found the Chicken!" "Good Robot!")
    (attroff (COLOR_PAIR *message-colour*))

    (attron (COLOR_PAIR *help-colour*))
    (mvprintw (- (LINES) 2) (- (COLS) 24) "Press R to play again.")
    (attroff (COLOR_PAIR *help-colour*))

    (flushinp)
    (refresh)
    (thread-sleep! 2)

    ;; Play again.
    (when (member (getch) '(#\r #\R))
      (clear)
      (endwin)
      (rfc-init #f))

    (beep)
    ;; Exit on any other key.
    (quit-game "Thanks for playing!" 0)))
