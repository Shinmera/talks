(in-package "CL-USER")
(ql:quickload :trial-assimp)
(in-package "els")
(use-package :cl+trial)

(define-pool els
  :base #.(pathname *load-truename*))

(define-asset (els skybox) image
    (list #p"sea-skybox/posx.tga"
          #p"sea-skybox/negx.tga"
          #p"sea-skybox/posy.tga"
          #p"sea-skybox/negy.tga"
          #p"sea-skybox/posz.tga"
          #p"sea-skybox/negz.tga")
  :target :texture-cube-map
  :min-filter :linear)

(define-asset (els white) image
    #p"white.png")

(define-asset (els black) image
    #p"black.png")

(define-asset (els neutral-normal) image
    #p"neutral-normal.png")

(define-asset (els building-a) mesh
    #p"obj/Building08.obj"
  :geometry-name "B_Set06_5_A")

(define-asset (els building-b) mesh
    #p"obj/Building08.obj"
  :geometry-name "B_Set06_5_B")

(define-asset (els building-a-albedo) image
    #p"obj/B_Set06_5_A_A.png"
  :internal-fromat :srgb)

(define-asset (els building-b-albedo) image
    #p"obj/B_Set06_5_B_A.jpg"
  :internal-fromat :srgb)

(define-asset (els building-a-metalness) image
    #p"obj/B_Set06_5_A_M.png")

(define-asset (els building-a-normal) image
    #p"obj/B_Set06_5_A_N.png")

(define-asset (els building-a-roughness) image
    #p"obj/B_Set06_5_A_R.png")

(define-subject show-camera (editor-camera)
  ())

(define-handler (show-camera pan tick) (ev dt tt)
  (setf (vx (rotation show-camera)) (* 0.1 (cos (/ tt 10))))
  (setf (vy (rotation show-camera)) (+ (* PI 0.7) (* 0.5 (sin (/ tt 5))))))

;; Pin so that we don't have (much) loading between slides
(defmethod compute-resources ((slide beamer:slide) resources readying cache)
  (dolist (pinned '(white black neutral-normal building-a building-b building-a-albedo building-b-albedo
                    building-a-metalness building-a-normal building-a-roughness))
    (vector-push-extend (asset 'els pinned) resources))
  (call-next-method))

(defmethod change-scene :after (main (slide beamer:slide) &key old)
  (declare (ignore old))
  (let ((buffer (asset 'trial:trial 'trial:light-block)))
    (when (gl-name buffer)
      (flet ((field (i field)
               (format NIL "LightBlock.lights[~d].~(~a~)" i field)))
        (setf (buffer-field buffer (field 0 'type)) 1)
        (setf (buffer-field buffer (field 0 'direction)) (nv- (vec 400 300 150)))
        (setf (buffer-field buffer (field 0 'color)) (v* (vunit (vec 9 8 5)) 10)))
      (setf (buffer-field buffer "LightBlock.count") 1))))

(define-shader-entity test (geometry-shaded located-entity scaled-entity)
  ())

(define-shader-pass deferred-fixup-pass (hdr-output-pass
                                         deferred-render-pass)
  ())

(define-class-shader (deferred-fixup-pass :fragment-shader 5)
  (gl-source (asset 'trial 'light-block))
  "
float lighting_strength = 1.0;
vec3 ambient_light = vec3(0.1);")

(define-shader-pass deferred+shadow-pass (high-color-pass
                                          hdr-output-pass
                                          deferred-render-pass
                                          shadow-render-pass)
  ((occlusion-map :port-type input)))

(define-class-shader (deferred+shadow-pass :fragment-shader 5)
  (gl-source (asset 'trial 'light-block))
  "in vec2 tex_coord;

uniform sampler2D position_map;
uniform sampler2D normal_map;
uniform sampler2D albedo_map;
uniform sampler2D occlusion_map;

float lighting_strength = 1.0;
vec3 ambient_light = vec3(0.1);
void main(){
  vec3 position = texture(position_map, tex_coord).rgb;
  vec3 normal = texture(normal_map, tex_coord).rgb;
  vec3 light_direction = light_block.lights[0].position-position;
  float bias = shadow_bias(normal, light_direction);
  float shadow = shadow_factor(position, bias);
  lighting_strength = 1-(0.9 * shadow);
  ambient_light *= texture(occlusion_map, tex_coord).r;
}")

(defun build-entities (scene)
  (enter (make-instance 'show-camera :LOCATION (VEC3 -178.21268 68.54084 121.44603)
                                     :ROTATION (VEC3 0.060004286 1.976846 0.0))
         scene)
  (flet ((add (vert diff spec norm rough ao &rest initargs)
           (enter (apply #'make-instance 'test
                         :specular-map (asset 'els spec)
                         :diffuse-map (asset 'els diff)
                         :normal-map (asset 'els norm)
                         :roughness-map (asset 'els rough)
                         :occlusion-map (asset 'els ao)
                         :vertex-array (asset 'els vert)
                         initargs)
                  scene)))
    (add 'building-a 'building-a-albedo 'building-a-metalness 'building-a-normal 'building-a-roughness 'white
         :scaling (vec 100 100 100)
         :location (vec -400 0 0))
    (add 'building-b 'building-b-albedo 'black 'neutral-normal 'black 'white
         :scaling (vec 100 100 100)
         :location (vec -400 0 0))))

(defun build-scene (scene &optional (part :full))
  (build-entities scene)
  (let* ((visualizer (make-instance 'visualizer-pass))
         (shadow (make-instance 'shadow-map-pass :projection-matrix (mortho -800 800 -800 800 1.0 2000)
                                                 :view-matrix (mlookat (vec 400 300 150) (vec 0 0 0) (vec 0 1 0))
                                                 :name :shadow-map-pass))
         (geometry (make-instance 'geometry-pass))
         (ssao (make-instance 'ssao-pass :uniforms `(("kernel_size" 16))))
         (lighting-s (make-instance 'deferred-fixup-pass))
         (lighting (make-instance 'deferred+shadow-pass :shadow-map-pass shadow))
         (h-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
         (v-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
         (h-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
         (v-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
         (skybox (make-instance 'skybox-pass :texture (asset 'els 'skybox)))
         (tone-map (make-instance 'bloom-pass))
         (blend (make-instance 'blend-pass)))
    (case part
      (:g-buffer
       (setf (uniforms visualizer) '(("textures_per_line" 2)
                                     ("channel_count[0]" 3)
                                     ("channel_count[1]" 3)
                                     ("channel_count[2]" 3)
                                     ("channel_count[3]" 3)))
       (connect (port geometry 'position) (port visualizer 't[0]) scene)
       (connect (port geometry 'normal) (port visualizer 't[1]) scene)
       (connect (port geometry 'albedo) (port visualizer 't[2]) scene)
       (connect (port geometry 'metal) (port visualizer 't[3]) scene)
       )
      (:deferred
       (connect (port geometry 'position) (port lighting-s 'position-map) scene)
       (connect (port geometry 'normal) (port lighting-s 'normal-map) scene)
       (connect (port geometry 'albedo) (port lighting-s 'albedo-map) scene)
       (connect (port geometry 'metal) (port lighting-s 'metal-map) scene)
       (connect (port lighting-s 'color) (port (make-instance 'tone-mapping-pass) 'previous-pass) scene))
      (:shadow-buffer
       (setf (uniforms visualizer) '(("channel_count[0]" 1)))
       (connect (port shadow 'shadow) (port visualizer 't[0]) scene))
      (:ssao
       (setf (uniforms visualizer) '(("channel_count[0]" 1)))
       (connect (port geometry 'position) (port ssao 'position-map) scene)
       (connect (port geometry 'normal) (port ssao 'normal-map) scene)
       (connect (port ssao 'occlusion) (port visualizer 't[0]) scene))
      (:deferred-full
       (connect (port geometry 'position) (port ssao 'position-map) scene)
       (connect (port geometry 'normal) (port ssao 'normal-map) scene)
       (connect (port shadow 'shadow) (port lighting 'shadow-map) scene)
       (connect (port geometry 'position) (port lighting 'position-map) scene)
       (connect (port geometry 'normal) (port lighting 'normal-map) scene)
       (connect (port geometry 'albedo) (port lighting 'albedo-map) scene)
       (connect (port geometry 'metal) (port lighting 'metal-map) scene)
       (connect (port ssao 'occlusion) (port h-blur2 'previous-pass) scene)
       (connect (port h-blur2 'color) (port v-blur2 'previous-pass) scene)
       (connect (port v-blur2 'color) (port lighting 'occlusion-map) scene)
       (connect (port lighting 'color) (port (make-instance 'tone-mapping-pass) 'previous-pass) scene))
      (:skybox
       (enter skybox scene))
      (:bloom
       (connect (port geometry 'position) (port ssao 'position-map) scene)
       (connect (port geometry 'normal) (port ssao 'normal-map) scene)
       (connect (port shadow 'shadow) (port lighting 'shadow-map) scene)
       (connect (port geometry 'position) (port lighting 'position-map) scene)
       (connect (port geometry 'normal) (port lighting 'normal-map) scene)
       (connect (port geometry 'albedo) (port lighting 'albedo-map) scene)
       (connect (port geometry 'metal) (port lighting 'metal-map) scene)
       (connect (port ssao 'occlusion) (port h-blur2 'previous-pass) scene)
       (connect (port h-blur2 'color) (port v-blur2 'previous-pass) scene)
       (connect (port v-blur2 'color) (port lighting 'occlusion-map) scene)
       (connect (port lighting 'high-pass) (port h-blur 'previous-pass) scene)
       (connect (port h-blur 'color) (port v-blur 'previous-pass) scene))
      (:full
       (connect (port geometry 'position) (port ssao 'position-map) scene)
       (connect (port geometry 'normal) (port ssao 'normal-map) scene)
       (connect (port shadow 'shadow) (port lighting 'shadow-map) scene)
       (connect (port geometry 'position) (port lighting 'position-map) scene)
       (connect (port geometry 'normal) (port lighting 'normal-map) scene)
       (connect (port geometry 'albedo) (port lighting 'albedo-map) scene)
       (connect (port geometry 'metal) (port lighting 'metal-map) scene)
       (connect (port ssao 'occlusion) (port h-blur2 'previous-pass) scene)
       (connect (port h-blur2 'color) (port v-blur2 'previous-pass) scene)
       (connect (port v-blur2 'color) (port lighting 'occlusion-map) scene)
       (connect (port lighting 'high-pass) (port h-blur 'previous-pass) scene)
       (connect (port h-blur 'color) (port v-blur 'previous-pass) scene)
       (connect (port v-blur 'color) (port tone-map 'high-pass) scene)
       (connect (port lighting 'color) (port tone-map 'previous-pass) scene)
       (connect (port skybox 'color) (port blend 'a-pass) scene)
       (connect (port tone-map 'color) (port blend 'b-pass) scene)))))

(define-slide title
  (note "Welcome everyone, explain talk difference")
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
  (build-scene beamer::*slide*))

(define-slide pipeline
  (note "Explain the pipeline as an overview.")
  (h "For such a scene we need:")
  (image #p"sample-pipeline.png" :margin (vec 300 10)))

(define-slide g-buffer
  (note "Show the various outputs of the g-buffer")
  (build-scene beamer::*slide* :g-buffer))

(define-slide rendering
  (note "Show the shadow buffer output")
  (build-scene beamer::*slide* :deferred))

(define-slide shadow-buffer
  (note "Show the shadow buffer output")
  (build-scene beamer::*slide* :shadow-buffer))

(define-slide ssao
  (note "Show the SSAO output")
  (build-scene beamer::*slide* :ssao))

(define-slide full-rendering
  (note "Show the renderer output")
  (build-scene beamer::*slide* :deferred-full))

(define-slide skybox
  (note "Show the skybox output")
  (build-scene beamer::*slide* :skybox))

(define-slide bloom
  (note "Show the bloom output")
  (build-scene beamer::*slide* :bloom))

(define-slide composite
  (note "Show the full pipeline again")
  (build-scene beamer::*slide* :full))

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
  (h "Issues")
  (items
   "Code analysis very primitive"
   "Cannot capture user intent"
   "Need to anticipate combination"))

(define-slide future-work
  (note "Show some ideas for things that could be done in the future.")
  (h "Future Work")
  (items
   "Code inference and analysis"
   "Use-relation tracking"
   "Shader I/O interface abstraction"))

(define-slide end
  (note "End slide, show the completed scene again.")
  (build-scene beamer::*slide*))

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

