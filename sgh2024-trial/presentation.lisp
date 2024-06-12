#| Make the slide executable
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload :beamer :silent T)" \
  --eval "(beamer:start-slideshow \"$0\" :muffle-logging T)" \
  --quit \
  --end-toplevel-options "${@:1}"
|#

(in-package #:beamer-user)
(ql:quickload '(trial-gltf trial-assets))

(define-pool presentation)

(define-asset (presentation player) model-file
    #p "~/Projects/cl/weiss/data/models/weiss.glb")

;; EDIT-1
(define-shader-entity player (basic-animated-entity listener)
  ())

(define-handler (player tick) (dt)
  (let ((vel (vec 0 0 0)))
    (when (retained :a) (decf (vx vel) 10.0))
    (when (retained :d) (incf (vx vel) 10.0))
    (setf (orientation player) (qfrom-angle +vy+ (* F-PI/2 (signum (vx vel)))))
    (fade-to (if (v/= vel 0) :run :idle) player)
    (nv+ (tlocation (tf player)) (nv* vel dt))))
;; EDIT-1

(define-handler (player tick :after) (dt)
  (handle tick (animation-controller player)))

(define-slide zero)

(define-slide hello
  (image "avatar.png" '(500 300))
  (h "HELLO" :size 80 :halign :center)
  (p "I'm Yukari Hafner" :size 48 :halign :center)
  (c "https://shinmera.com" :halign :center))

(define-slide me
  (h "What I Do")
  (items "Full-time indie"
         "Open Source developer"
         "Artist"
         "etc"))

(define-slide trial
  (image "trial.png" '(500 250) :padding (alloy:margins 50))
  (items "Written entirely in Common Lisp"
         "Programming-focused engine"
         "PC Support, Nintendo Switch soon"
         "Everything can be changed while the game is running"))

(define-slide example
  (h "An example")
  (editor "presentation.lisp" :start ";; EDIT-1" :end ";; EDIT-1" :language :lisp)
  (enter-instance 'directional-light)
  (enter-instance 'ambient-light :color (vec 0.5 0.5 0.5))
  (enter-instance 'player :asset (asset 'presentation 'player))
  (enter-instance 'target-camera :target (vec 0 3 0) :location (vec 0 3 4))
  (enter-instance 'pbr-render-pass))

(define-slide runtime
  (h "So what can be changed at runtime?")
  (p "Everything!")
  (items "Variables"
         "Assets"
         "Shaders"
         "Classes"
         "Methods"
         "What class an object is an instance of!"))

(define-slide performance
  (h "Performance")
  (items "Lisp code is compiled to machine code"
         "Can give the compiler lots of hints on how to optimise"
         "Can even customise the compiler itself if need-be"
         "All while remaining interactive"))

(define-slide features
  (h "Trial Features")
  (items "PBR rendering"
         "3D physics system"
         "Integration with Steam, Blender, Aseprite"
         "Broad image/audio format support"
         "Automated release management"
         "Automated crash reports"))

(define-slide learn-more
  (h "Where to Learn More?")
  (items "Learning Lisp: https://gigamonkeys.com/book"
         "Learning Trial: https://shirakumo.org/docs/trial"
         "Talk to me!"))

(define-slide buy
  (image "kandria.png" '(1920 500))
  (c "https://kandria.com" :halign :center :size 50))
