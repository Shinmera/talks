(in-package "els")
(use-package :cl+trial)

(define-pool els
  :base :trial)


(defun build-scene (&rest parts)
  (setf parts (or parts '()))
  )

;; TODO: Mark assets as "always needed" to reduce load time

(define-slide title
  (note "Explain difficulties")
  (h "Modular Graphics Pipelines" :size 62 :margin (vec 30 200))
  (c "https://shinmera.com" :language :link :size 32 :margin (vec 220 30)))

(define-slide overview
  (note "Explain steps of the talk.")
  (h "Talk Overview")
  (items :bullet NIL
   "0. Terminology"
   "1. What we want to get"
   "2. What's needed to get it"
   "3. How to build it"
   "4. Challenges"))

(define-slide terminology
  (note "Briefly explain basic terminology.")
  (h "Terminology")
  (items
   "Shader: code run on the GPU for rendering"
   "Pass: a GPU rendering invocation"
   "Pipeline: a series of passes"
   "Texture: an image buffer on the GPU"))

(define-slide preview
  (note "Explain the sample scene and actually show it.")
  (build-scene))

(define-slide pipeline
  (note "Explain the pipeline as an overview.")
  (h "For such a scene we need:")
  (image #p"sample-pipeline.png" :margin (vec 300 10)))

(define-slide g-buffer
  (note "Show the various outputs of the g-buffer")
  (build-scene :g-buffer))

(define-slide shadow-buffer
  (note "Show the shadow buffer output")
  (build-scene :shadow-buffer))

(define-slide ssao
  (note "Show the SSAO output")
  (build-scene :g-buffer :ssao))

(define-slide rendering
  (note "Show the renderer output")
  (build-scene :g-buffer :ssao :shadow-buffer :deferred))

(define-slide skybox
  (note "Show the skybox output")
  (build-scene :skybox))

(define-slide bloom
  (note "Show the bloom output")
  (build-scene :g-buffer :ssao :shadow-buffer :deferred :bloom))

(define-slide composite
  (note "Show the full pipeline again")
  (build-scene))

(define-slide challenges
  (note "Explain what the common challenges are")
  (h "Architectural Challenges")
  (items
   "Pipelines become complex"
   "Lots of buffer state to manage"
   "Many parts interact tightly"))

(define-slide abstract-pipelines
  (note "By abstracting the pipelines we can automate allocation")
  (h "Abstract Pipelines")
  (c ";; Create a new pass
(define-shader-pass ssao-pass (post-effect-pass)
  ((depth     :port-type input 
              :texspec (:internal-format :depth-component))
   (normal    :port-type input)
   (occlusion :port-type output
              :texspec (:internal-format :red))))

;; Add shader code
(define-class-shader (ssao-pass :fragment-shader)
  \"...\")

;; Connect passes into a pipeline
(let ((pipeline (make-instance 'pipeline))
      (g-buffer (make-instance 'g-buffer-pass))
      (ssao-pass (make-instance 'ssao-pass)))
  (connect (port g-buffer 'depth) (port ssao-pass 'depth))
  (connect (port g-buffer 'normal) (port ssao-pass 'normal)))" :language :lisp :size 26))

(define-slide automated-allocation
  (note "The system will automatically infer the optimal texture allocation for the pipeline.")
  (h "Automated Buffer Allocation")
  ;; TODO: Add snippet
  )

(define-slide modular-shaders
  (note "We solve the modularity problem by shader composition through classes. The shader combination is done once at scene setup.")
  (h "Modular Shader Composition")
  (c ";; Create passes through automated combination
(define-shader-pass renderer (high-color-pass 
                              hdr-output-pass 
                              deferred-render-pass 
                              shadow-render-pass)
  ())" :language :lisp :size 26))

(define-slide issues
  (note "Explain what the issues are with the current approach.")
  (items
   "Code analysis very primitive"
   "Cannot capture user intent"
   "Need to anticipate combination"))

(define-slide future-work
  (note "Show some ideas for things that could be done in the future.")
  (items
   "Code inference and analysis"
   "Use-relation tracking"
   "Shader I/O interface abstraction"))

(define-slide end
  (note "End slide, show the completed scene again.")
  (build-scene))

;;; Backup slides
#-(and)
(progn
  (define-slide opengl-pipeline
    (h "Fixed Rendering Pipeline")
    (image #p"pipeline.png" :margin (vec 10 100)))

  (define-slide opengl-pipeline-input
    (h "Pipeline Input")
    (image #p"pipeline-input.png" :margin (vec 10 100)))

  (define-slide opengl-pipeline-data
    (h "Pipeline Data")
    (image #p"pipeline-data.png" :margin (vec 10 100))))

