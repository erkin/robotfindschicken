#!/usr/bin/csi -s
(use ncurses)
(require-extension srfi-9)
(require-extension srfi-13)
(require-extension srfi-18)

(include "const.scm")
(include "draw.scm")
(include "move.scm")
(include "game.scm")

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
