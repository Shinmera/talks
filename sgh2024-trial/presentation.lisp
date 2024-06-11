#| Make the slide executable
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload '(beamer) :silent T)" \
  --eval "(beamer:start-slideshow \"$0\" :muffle-logging T)" \
  --quit \
  --end-toplevel-options "${@:1}"
|#

(in-package #:beamer-user)

(define-slide hello
  (h "HELLO")
  (image "avatar.png")
  (p "Yukari Hafner")
  (c "https://shinmera.com"))

(define-slide me
  (h "What I Do")
  (items "Full-time indie"
         "Open Source developer"
         "Artist"
         "etc"))

(define-slide trial
  (image "trial.png")
  (items "Written entirely in Common Lisp"
         "Programming-focused engine"
         "PC Support, Nintendo Switch soon"
         "Works well for 2D, 3D in the works"
         "Everything can be changed while the game is running"))

(define-slide example
  ())
