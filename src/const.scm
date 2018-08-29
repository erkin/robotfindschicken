;;; TODO: come up with a better name and grouping
(declare (unit rfc-const))

(module rfc-const *
  (import chicken scheme extras)
  (use ncurses)
  
;;;; Preliminary procedures

;;; Run the procedure f n times
  (define (repeat f n)
    (if (= n 1)
        (f)
        ((lambda ()
           (f)
           (repeat f (sub1 n))))))


;;;; Constant values

  (define-constant *frame-colour*   1)
  (define-constant *decor-colour*   2)
  (define-constant *chicken-colour* 3)
  (define-constant *message-colour* 4)
  (define-constant *help-colour*    5)
  (define-constant *control-colour* 6)
  (define-constant *extra-colour*   7)
  (define-constant *robot-colour*   8)

  (define-constant messages
    '("A +0 uncursed cockatrice corpse."
      "A 3D model of a bunny containing 69451 polygons. You do not feel like verifying that claim."
      "A B+ tree in full bloom."
      "A LART made from depleted uranium."
      "A bottle of a soft drink. Warm and flat."
      "A cheque from Donald Knuth, for one hexadecimal dollar."
      "A defiled altar formerly dedicated to the god of IT security."
      "A deserted coop."
      "A fake Amulet of Yendor."
      "A finite state machine. It appears to have stopped. Problematic."
      "A fork that once belonged to one of the dining philosophers."
      "A gift-wrapped set of straightedge and compasses."
      "A handheld electronic console 'Game of Life'. Glidey!"
      "A picture of a woman wearing a hat with feathers. Compression artifacts distorted her face but it looks familiar."
      "A pocket sized copy of the Chine Nual. It's thicker than it's long."
      "A potted plant of dubious legality. It needs watering."
      "A semi-automatic, conservatory key system oboe."
      "A well-worn set of Slackware 1.0.0 installation floppy set."
      "An ANSI-art picture of a naked man printed with a dot-matrix printer."
      "An ASCII-art picture of a naked person printed with a daisy-wheel printer."
      "An EBCDIC-art picture of a naked woman printed with a line printer."
      "An action figure of Guy Steele, 1:18 scale."
      "An electric cdr."
      "An empty set. It's barely visible."
      "An old keyboard. The hyper key seems to be missing."
      "Do you want your possessions identified? [ynq]"
      "Five tonnes of flax! Or is it hemp?"
      "It's a doorstopper with no door to stop."
      "It's a sleeping daemon."
      "One of Paul Graham's oil paintings."
      "There is absolutely nothing here.                       (fnord?)"
      "These are two bodies connected by a weightless string."
      "This is a Canterbury corpus in folio, bound in leather of Stanford bunnies."
      "This is a VAXServer running OpenVMS 8.3."
      "This is a cheap knock-off of the platinum-iridium kilogramme standard."
      "This is a sleeping barber. Don't touch his locks."
      "This is a teapot. You are certain that you've seen it somewhere."
      "This is almost a chicken, but not quite. Try harder."
      "You found some slack."
      "You have no idea what this it, but you hope it's not what it looks like."))

  (define-constant chars
    '(#\! #\@ #\# #\$ #\% #\& #\*
      #\/ #\= #\? #\+ #\- #\_ #\\
      #\| #\' #\, #\. #\; #\: #\"
      #\< #\> #\` #\~))


;;;; Record definitions

  (define-record item
    row col row-prev col-prev
    moved? message colour char)

  (define-record-type item
    (spawn-item item-row item-col
                item-row-prev item-col-prev
                moved item-message
                item-colour item-char)
    item?
    (item-row row row-set!)
    (item-col col col-set!)
    (item-row-prev row-prev row-prev-set!)
    (item-col-prev col-prev col-prev-set!)
    (item-message message message-set!)
    (item-colour colour colour-set!)
    (item-char char char-set!)
    (moved moved? moved!))


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
        (cons (spawn-item
               (random-row) (random-col) 0 0
               #f (random-elem messages)
               (add1 (random 7))        ; Random colour
               (random-elem chars))
              (generate-items (sub1 count)))
        '()))

  (define (quit-game message code)
    ;; Stop input to avoid cluttering the terminal
    (flushinp)
    (attroff A_BOLD)
    (clear)
    (endwin)
    (unless (zero? code)
      (fprintf (current-error-port) message)
      (exit code))                      ; quit with an error
    (print message)
    (exit)))
