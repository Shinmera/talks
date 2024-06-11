#| Make the slide executable
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload '(beamer trial-glfw) :silent T)" \
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
         "Everything can be changed while the game is running"))

(define-pool presentation)
(define-asset (presentation player) model-file
    #p "~/Projects/cl/weiss/data/models/weiss.glb")

;; EDIT-1
(define-shader-entity player (basic-animated-entity listener)
  ())

(define-handler (player tick) (dt)
  (let ((vel (vec 0 0 0)))
    (when (retained :a) (incf (vx vel) 10.0))
    (when (retained :d) (decf (vx vel) 10.0))
    (fade-to (if (v/= vel 0) :running :idle) player)
    (nv+ (tlocation (tf player)) (nv* vel dt))))
;; EDIT-1

(define-handler (player tick :after) (dt)
  (handle tick (animation-controller player)))

(define-slide example
  (h "An example")
  (editor "presentation.lisp"
          :start ";; EDIT-1"
          :end   ";; EDIT-1"
          :language :lisp)
  (enter-instance 'ambient-light :color (vec 0.5 0.5 0.5))
  (enter-instance 'player :asset (asset 'presentation 'player))
  (enter-instance 'target-camera :target (vec 0 2 0) :location (vec 0 3 5))
  (enter-instance 'pbr-render-pass))
