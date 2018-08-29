(declare (unit rfc-game))
(declare (uses rfc-const))
(declare (uses rfc-draw))

(module rfc-game *
  (import chicken scheme data-structures)
  (import (only extras fprintf))
  (require-extension (only srfi-18 thread-sleep!))
  (require-extension (only srfi-13 string-take))
  (use ncurses)

  (import rfc-const rfc-draw)

;;;; Global values

  (define items '())
  (define item-count #f)
  (define robot #f)
  (define chicken #f)


;;;; Movement procedures

  (define (check-position return)
    (define (check-item-position items)
      (when (pair? items)
        (let ((item (car items)))
          ;; You bumped into an item
          (when (and (= (row item) (row robot))
                     (= (col item) (col robot)))
            ;; Clip the message string if it's too long.
            (if (>= (string-length (message item)) (- (COLS) 3))
                (begin
                  (mvprintw 1 2 (string-take (message item) (- (COLS) 5)))
                  (addch (ACS_RARROW)) (addch (ACS_RARROW)))
                (mvprintw 1 2 (message item))) ; Print item's message
            (row-set! robot (row-prev robot))
            (col-set! robot (col-prev robot))
            (return #f)))
        (check-item-position (cdr items)))) ; Or was it another item?

    ;; Is it the chicken?
    (if (and (= (col chicken) (col robot))
             (= (row chicken) (row robot)))
        ;; Way to go!
        (rfc-win))

    ;; Was it an item?
    ;; Let's inefficiently check all of them.
    (check-item-position items)

    ;; Nope, you're clear.
    (return #t))


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
    (moved! robot
            (call-with-current-continuation
             (lambda (return) (check-position return)))))


;;;; Game sections

  (define (rfc-splash)
    (define (draw-layout line keys spacing)
      (mvprintw (- (LINES) line) 6
                (apply string-append (intersperse (map (compose ->string layout-ref) keys) spacing))))
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
    (if (member (getch) '(#\q #\Q KEY_F0))
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
    (switch-layout layout)

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

    ;; (case) does not evaluate values...
    (select (getch)
      ((#\q #\Q KEY_F0)
       (quit-game "You couldn't find the Chicken. Sad!" 0))
      (((layout-ref 'up-char) KEY_UP)
       (move-robot v: 'up))
      (((layout-ref 'up-right-char))
       (move-robot v: 'up h: 'right))
      (((layout-ref 'right-char) KEY_RIGHT)
       (move-robot h: 'right))
      (((layout-ref 'down-right-char))
       (move-robot v: 'down h: 'right))
      (((layout-ref 'down-char) KEY_DOWN)
       (move-robot v: 'down))
      (((layout-ref 'down-left-char))
       (move-robot v: 'down h: 'left))
      (((layout-ref 'left-char) KEY_LEFT)
       (move-robot h: 'left))
      (((layout-ref 'up-left-char))
       (move-robot v: 'up h: 'left))
      ((#\? KEY_HELP)
       (rfc-splash)))

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

    ;; (attron (COLOR_PAIR *help-colour*))
    ;; (mvprintw (- (LINES) 2) (- (COLS) 24) "Press R to play again.")
    ;; (attroff (COLOR_PAIR *help-colour*))

    (flushinp)
    (refresh)
    (thread-sleep! 1)

    ;; ;; Play again.
    ;; (when (member (getch) '(#\r #\R))
    ;;   (clear)
    ;;   (endwin)
    ;;   (rfc-init))

    ;; Exit on any other key.
    (quit-game "Thanks for playing!" 0)))
