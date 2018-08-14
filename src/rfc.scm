#!/usr/bin/csi -ss
(use ncurses)
(require-extension srfi-13)
(require-extension srfi-18)

(define (repeat f n) ; run the procedure f n times
  (if (= n 1)
      (f)
      ((lambda ()
         (f)
         (repeat f (sub1 n))))))
(define (cadddar lst)
  (car (cdddar lst)))

(define-constant FRAME_COLOUR   1)
(define-constant DECOR_COLOUR   2)
(define-constant CHICKEN_COLOUR 3)
(define-constant MESSAGE_COLOUR 4)
(define-constant HELP_COLOUR    5)
(define-constant CONTROL_COLOUR 6)
(define-constant EXTRA_COLOUR   7)
(define-constant ROBOT_COLOUR   8)

(define moved? (make-parameter #f))

(define robot-row (make-parameter 1))
(define robot-col (make-parameter 1))

(define robot-row-prev (make-parameter 1))
(define robot-col-prev (make-parameter 1))

(define chicken-row (make-parameter 1))
(define chicken-col (make-parameter 1))

(define-constant messages
  '("this is a foo"
    "this is a bar"
    "this is a qux"))

(define stuff '())

(define (random-row)
  (+ 3 (random (- (LINES) 5))))
(define (random-col)
  (+ 1 (random (- (COLS) 2))))
(define (random-message)
  (list-ref messages (random (length messages))))

;; Generate a number of locations to place non-chicken items
(define (generate-stuff count)
  (if (not (zero? count))
      (cons (list (random-row) (random-col)
                  (random-message) (add1 (random 7)))
            (generate-stuff (sub1 count)))
      '()))

(define (quit-game message code)
  (flushinp) ; Stop input to avoid cluttering the terminal
  (attroff A_BOLD)
  (clear)
  (endwin)
  (unless (zero? code)
    (fprintf (current-error-port) message)
    (exit code))
  (print message)
  (exit))

(define (centre-message . messages) ; Write messages in the middle of the window
  (define (centre-message-iter messages n) ; with doublespacing
    (when (pair? messages)
      (mvprintw (+ (quotient (LINES) 2) (- n (length messages)))
                (quotient (- (COLS) (string-length (car messages))) 2)
                (car messages))
      (centre-message-iter (cdr messages) (add1 n))))
  (centre-message-iter messages 0))

(define (draw-stuff stuff-alist)
  (unless (null? stuff-alist)
    (let ((item (car stuff-alist)))
      (attron (COLOR_PAIR (cadddr item)))
      (mvaddch (car item) (cadr item) #\@)
      (attroff (COLOR_PAIR (cadddr item))))
    (draw-stuff (cdr stuff-alist))))

(define (draw-chicken)
  (attron (COLOR_PAIR CHICKEN_COLOUR))
  (mvaddch (chicken-row) (chicken-col) (ACS_CKBOARD))
  (attroff (COLOR_PAIR CHICKEN_COLOUR)))

(define (draw-robot)
  (mvaddch (robot-row-prev) (robot-col-prev) #\ ) ; remove the previous robot
  (attron (COLOR_PAIR ROBOT_COLOUR))
  (mvaddch (robot-row) (robot-col) (ACS_DIAMOND))
  (attroff (COLOR_PAIR ROBOT_COLOUR))
  (moved? #f))

(define (check-position return)
  (define (check-item-position stuff)
    (if (pair? stuff)
        (begin
          (let ((item (car stuff)))
            (when (and (= (car  item) (robot-row))
                       (= (cadr item) (robot-col)))
              (mvprintw 1 2 (caddr item)) ; You bumped into an item.
              (robot-row (robot-row-prev))
              (robot-col (robot-col-prev))
              (return #f)))
          (check-item-position (cdr stuff)))
        (return #t)))
  
  (if (and (= (chicken-col) (robot-col)) ; You found chicken!
           (= (chicken-row) (robot-row)))
      (rfc-win))

  (check-item-position stuff))

(define (move-robot #!key (v 'nil) (h 'nil))
  (mvprintw 1 2 (make-string (- (COLS) 35) #\ ))
  (robot-row-prev (robot-row))
  (robot-col-prev (robot-col))

  (if (eq? v 'up) ; Vertical movement
      (robot-row (sub1 (robot-row)))
      (if (eq? v 'down)
          (robot-row (add1 (robot-row)))))

  (if (eq? h 'left) ; Horizontal movement
      (robot-col (sub1 (robot-col)))
      (if (eq? h 'right)
          (robot-col (add1 (robot-col)))))

  (if (= (robot-row) (- (LINES) 1)) ; Vertical boundary check
      (robot-row (- (LINES) 2))
      (if (= (robot-row) 2)
          (robot-row 3)))

  (if (= (robot-col) (- (COLS) 1)) ; Horizontal boundary check
      (robot-col (- (COLS) 2))
      (if (= (robot-col) 0)
          (robot-col 1)))

  (moved? (call-with-current-continuation
           (lambda (return) (check-position return)))))

(define (rfc-frame)
  (attron (COLOR_PAIR FRAME_COLOUR))
  (box (stdscr) (ACS_VLINE) (ACS_HLINE))
  (mvhline 2 1 (ACS_HLINE) (- (COLS) 2))
  (mvaddch 2 0 (ACS_LTEE))
  (mvaddch 2 (sub1 (COLS)) (ACS_RTEE))
  (attroff (COLOR_PAIR FRAME_COLOUR))

  (attron (COLOR_PAIR DECOR_COLOUR))
  (mvprintw 2 2 "robotfinds")
  (attroff (COLOR_PAIR DECOR_COLOUR))

  (attron (COLOR_PAIR CHICKEN_COLOUR))
  (printw "chicken")
  (attroff (COLOR_PAIR CHICKEN_COLOUR)))

(define (rfc-splash)
  (rfc-frame)
  ;; The ordering is important to avoid rewriting
  ;; some parts with spaces.

  (attron (COLOR_PAIR HELP_COLOUR))
  (mvprintw (- (LINES) 2) (- (COLS) 18) "Press Q to exit.")
  (mvprintw (- (LINES) 3) 2 "Use       to move.")
  (mvprintw 3 3 "Press any key to begin/resume.")
  (attroff (COLOR_PAIR HELP_COLOUR))
  
  (attron (COLOR_PAIR CONTROL_COLOUR))
  (mvprintw (- (LINES) 4) 6 "7 8 9")
  (mvprintw (- (LINES) 3) 6 "4   6")
  (mvprintw (- (LINES) 2) 6 "1 2 3")
  (attroff (COLOR_PAIR CONTROL_COLOUR))

  (attron (COLOR_PAIR MESSAGE_COLOUR))
  (mvaddch (- (LINES) 3) 8 (ACS_PLUS))
  (attroff (COLOR_PAIR MESSAGE_COLOUR))
  
  (if (member (getch) '(#\q #\Q KEY_F0)) ; Mock the player for
      (quit-game "That was quick." 0)) ; exiting on help screen.

  (clear) ; Now that the player pressed a key, clear the screen

  (rfc-frame) ; And redraw the game elements
  (draw-stuff stuff)
  (draw-robot)
  (draw-chicken))

(define (rfc-init)
  (initscr)

  (if (not (has_colors))
      (quit-game "Your terminal does not support colours." 1))

  (start_color)
  (init_pair FRAME_COLOUR COLOR_BLUE COLOR_BLACK)
  (init_pair DECOR_COLOUR COLOR_CYAN COLOR_BLACK)
  (init_pair CHICKEN_COLOUR COLOR_RED COLOR_BLACK)
  (init_pair MESSAGE_COLOUR COLOR_YELLOW COLOR_BLACK)
  (init_pair HELP_COLOUR COLOR_GREEN COLOR_BLACK)
  (init_pair CONTROL_COLOUR COLOR_MAGENTA COLOR_BLACK)
  (init_pair EXTRA_COLOUR COLOR_BLUE COLOR_BLACK)
  (init_pair ROBOT_COLOUR COLOR_GREEN COLOR_BLUE)

  (set! stuff (generate-stuff (quotient (COLS) 5)))

  (raw) (noecho)
  (keypad (stdscr) #t)
  (curs_set 0)

  (attron A_BOLD)

  (robot-row (random-row)) ; Random initial positions
  (robot-col (random-col))
  (robot-row-prev (robot-row))
  (robot-col-prev (robot-col))
  (chicken-row (random-row))
  (chicken-col (random-col))

  (attron (COLOR_PAIR MESSAGE_COLOUR))
  (centre-message "You are the Robot!" "Find the Chicken!" "Godspeed!")
  (attroff (COLOR_PAIR MESSAGE_COLOUR))

  (rfc-splash)

  (mvprintw 1 (- (COLS) 32) "Press H any time to view help.")
  (rfc-loop))

(define (rfc-loop)
  (if (moved?) ; Only redraw if the action taken was movement.
      (draw-robot)) ; Other keys are ignored within the loop.

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
  (clear)
  (flushinp) ; stop taking input
  (rfc-frame)

  (mvprintw 1 (- (COLS) 9) "Aww...")
  
  (robot-row 1)
  (robot-col (+ (quotient (COLS) 2) 4))
  
  (chicken-row 1)
  (chicken-col (- (quotient (COLS) 2) 5))

  (draw-robot)
  (draw-chicken)
  (draw-stuff stuff)

  (repeat
   (lambda ()
     (thread-sleep! 0.8)
     (move-robot h: 'left)
     (chicken-col (add1 (chicken-col)))
     (draw-robot)
     (draw-chicken)
     (mvaddch (chicken-row) (sub1 (chicken-col)) #\ )
     (refresh))
   4)

  (thread-sleep! 0.5)

  (attron (COLOR_PAIR MESSAGE_COLOUR))
  (centre-message "You found the Chicken!" "Good Robot!")
  (attroff (COLOR_PAIR MESSAGE_COLOUR))

  (attron (COLOR_PAIR HELP_COLOUR))
  (mvprintw (- (LINES) 2) (- (COLS) 24) "Press R to play again.")
  (attroff (COLOR_PAIR HELP_COLOUR))

  (refresh)
  (thread-sleep! 1)
  (when (member (getch) '(#\r #\R))
    (clear)
    (endwin)
    (rfc-init))
  (quit-game "Thanks for playing!" 0))

(define (main args)  
  (if (null? args)
      (rfc-init)
      (print "This program does not take options.")))

(main (command-line-arguments))
