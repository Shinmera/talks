(define-module "D634ED63-6351-3A4B-686E-A59332DACDF0")

(defclass text (trigger listener creatable alloy:popup alloy:label*)
  ((alloy:value :initform "<Untitled>" :accessor text :type string)
   (h-align :initarg :h-align :initform :left :accessor h-align :type (member :left :center :right))
   (v-align :initarg :v-align :initform :top :accessor v-align :type (member :top :center :bottom))
   (font-size :initarg :font-size :initform 20 :accessor font-size :type integer)
   (offset :initarg :offset :initform (vec 0 0) :accessor offset)
   (markup :initarg :markup :initform '((0 10000 (:outline 1.0))) :accessor markup)
   (clock :initform 0.0 :accessor clock)))

(define-additional-slot-coders (text world-v0) (alloy:value font-size h-align v-align (offset :type vec2)))

(defmethod initargs append ((text text))
  '(:offset :font-size :h-align :v-align))

(presentations:define-realization (org.shirakumo.fraf.trial.alloy:ui text)
  ((label simple:text)
   (alloy:margins) alloy:text
   :font (setting :display :font)
   :size (alloy:un (font-size alloy:renderable))
   :wrap T
   :valign (v-align alloy:renderable)
   :halign (h-align alloy:renderable)
   :markup (markup alloy:renderable)
   :pattern colors:white))

(presentations:define-update (org.shirakumo.fraf.trial.alloy:ui text)
  (label
   :size (alloy:un (font-size alloy:renderable))
   :valign (v-align alloy:renderable)
   :halign (h-align alloy:renderable)
   :markup (markup alloy:renderable)
   :text alloy:text))

(defmethod interact ((text text) entity)
  (setf (clock text) 0.5)
  (unless (alloy:layout-tree text)
    (alloy:enter text (u 'ui-pass))))

(defmethod alloy:text ((text text))
  (alloy:value text))

(defmethod alloy:handle :around ((ev alloy:pointer-event) (text text))
  (alloy:decline))

(defmethod handle ((ev tick) (text text))
  (when (and (alloy:layout-tree text))
    (if (<= (decf (clock text) (dt ev)) 0.0)
        (alloy:leave text T)
        (alloy:with-unit-parent text
          (alloy:mark-for-render text)
          (let* ((screen-location (world-screen-pos (v+ (location text) (offset text))))
                 (z (* (view-scale (camera +world+)) (zoom (camera +world+))))
                 (w (* z (vx (bsize text)))) (h (* z (vy (bsize text)))))
            (setf (alloy:bounds text) (alloy:px-extent (- (vx screen-location) w)
                                                       (- (vy screen-location) h)
                                                       (* 2 w) (* 2 h))))))))
