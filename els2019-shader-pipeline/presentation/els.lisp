(in-package "els")
(use-package :cl+trial)

(define-pool els
  :base :trial)

(define-slide title
  (h "Modular Graphics Pipelines" :size 62 :margin (vec 30 200))
  (c "https://shinmera.com" :language :link :size 32 :margin (vec 220 30)))

(define-slide intro-image
  (image #p"reddead.jpg" :margin (vec 0 0)))

(define-slide intro-image2
  (image #p"reddead-2.jpg" :margin (vec 0 0)))

(define-slide intro-image3
  (image #p"witcher3.jpg" :margin (vec 0 0)))

(define-slide opengl
  (image #p"opengl.png" :margin (vec 200 100)))

(define-slide opengl-more
  (image #p"opengl.png" :margin (vec 200 0))
  (items
   "Standardised rendering toolkit"
   "Works on all desktop platforms*"
   "Gives access to GPU computing"
   "Intended for rendering triangles")
  (p "* Apple deprecated support in 2018" :margin (vec 0 150) :size 20))

(define-slide opengl-intro
  (h "Things OpenGL Gives You")
  (items
   "GPU-Data management"
   "GPU-Code language and framework"
   "A fixed rendering pipeline"))

(define-slide opengl-data
  (h "GPU-Data management")
  (items
   "Vertex arrays"
   "Textures"
   "Data buffers"))

(define-slide opengl-data2
  (h "GPU-Data management")
  (items
   "Vertex arrays"
   "Textures"
   "Data buffers")
  (p "Data must be allocated and uploaded before rendering."))

(define-slide opengl-code
  (h "GPU-Code language")
  (items
   "C-like language executed on GPU"
   "No pointers"
   "Extra, graphics-related features"))

(define-slide opengl-pipeline
  (h "Fixed Rendering Pipeline")
  (image #p"pipeline.png" :margin (vec 0 100)))
