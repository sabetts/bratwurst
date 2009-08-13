;;; bratwurst.lisp --- An implementation of the classic amiga game

;; Copyright (C) 2006,2009 Shawn Betts

;; Author: Shawn Betts <sabetts@gmail.com>
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; you can play up to 4 players but i've only added keys on the kbd
;; for 2. the keys are:
;;
;; player 1           player 2
;; --------           --------
;;   c                  ,
;; h t n Space        a o e  control
;; 
;; in network mode everyone uses player 1's controls.
;;
;; $Revision: 1.7 $
;;
;;; Code:

(in-package #:bratwurst)

(defvar *resource-dir* #p"/Users/sabetts/src/bratwurst/")

(defvar *ready* '("Play" "Eat" "Swim" "Rock"))

(defvar *frame-rate* 30)

(defvar *display* nil)
(defvar *window* nil)
(defvar *screen* nil)
(defvar *root* nil)
(defvar *font* nil)
(defvar *lives-font* nil)
(defvar *status-buffer* nil)

;; (defvar *status-bar-needs-updating* nil
;;   "Set to T when status changes.")

(defvar *player1-color* (sdl:color :r 255 :g 0 :b 0))
(defvar *player2-color* (sdl:color :r 0 :g 255 :b 0))
(defvar *player3-color* (sdl:color :r 0 :g 0 :b 255))
(defvar *player4-color* (sdl:color :r 255 :g 255 :b 0))

(defvar *map-max-zoom* 100)

;; status gcs
(defvar *text-color* (sdl:color :r 255 :g 255 :b 255))

(defstruct point x y)

(defstruct gun
  x y angle)

(defstruct rect-piece
  x y width height)

(defstruct circle-piece
  x y radius)

(defstruct balloon
  x y radius)

(defstruct (game-map (:conc-name map-))
  starts
  pieces)

(defstruct ship
  name
  shape
  guns
  fire-rate
  special-pts
  specials
  jet-pts
  speed
  max-ammo
  max-health
  max-special
  collision-balloons)

(defstruct controls
  id left right forward shoot special)

(defun controls-list (controls)
  "return controls as a list"
  (list 
   (controls-id controls)
   (controls-left controls)
   (controls-right controls)
   (controls-forward controls)
   (controls-special controls)
   (controls-shoot controls)))

(defvar *player-1-keys*
  (make-controls :left :sdl-key-left
                 :right :sdl-key-right
                 :forward :sdl-key-up
                 :special :sdl-key-lshift
                 :shoot :sdl-key-space))

(defvar *player-2-keys*
  (make-controls :left :sdl-key-a
                 :right :sdl-key-e
                 :forward :sdl-key-comma
                 :special :sdl-key-o
                 :shoot :sdl-key-semicolon))

(defvar *player-3-keys*
  (make-controls))

(defvar *player-4-keys*
  (make-controls))

(defstruct player
  controls
  color
  ship
  start-x start-y
  last-x
  last-y
  last-angle
  angle x y vx vy max-vel
  health
  ammo
  special
  (cool-down 0)
  special-type
  special-active
  (special-cooldown 0)
  special-gun
  lives
  ;; set to T when the player collided with the map. These players
  ;; cannot be moved for player-player collisions.
  solidified
  ;; this is the last non-colliding spot we found
  (last-good (make-gun :x 0 :y 0 :angle 0)))

(defstruct bullet
  x y vx vy color)

(defstruct (projectile (:include bullet))
  angle)

(defstruct (missile (:include projectile))
  target timer)

(defstruct (rocket (:include projectile))
  owner)

(defstruct ship-selection
  ship special special-pt confirm wait)

(defvar *state* nil
  "The game state.")

(defstruct state
  bullets missiles rockets
  status-bar-needs-updating
  players map
  frame)

;; i dont know how time consuming this is but it conses a lot that's
;; for sure.
(defun backup-state (state)
  (let ((bk-state (make-state
		   ;; backup all the dynamic objects
		   :bullets (mapcar 'copy-structure (state-bullets state))
		   :missiles (mapcar 'copy-structure (state-missiles state))
		   :rockets (mapcar 'copy-structure (state-rockets state))
		   :status-bar-needs-updating (state-status-bar-needs-updating state)
		   :players (mapcar 'copy-structure (state-players state))
		   ;; this stay the same
		   :map (state-map state))))
    ;; rockets have a link back to their owner so update that
    (loop for i in (state-rockets bk-state) do
	 (setf (rocket-owner i) (nth (position (rocket-owner i) (state-players state)) (state-players bk-state))))
    bk-state))

(defun make-empty-controls-array (n)
  (coerce (loop for i from 0 below n
	     collect (make-controls))
	  'vector))

(defun copy-controls-to (controls into)
  (loop for i from 0 below (length controls) do
       (let ((a (aref into i))
	     (b (aref controls i)))
	 (setf (controls-left a) (controls-left b)
	       (controls-right a) (controls-right b)
	       (controls-forward a) (controls-forward b)
	       (controls-special a) (controls-special b)
	       (controls-shoot a) (controls-shoot b)))))
	 
;; (defvar *bullets* nil
;;   "List of bullets to shoot.")

;; (defvar *rockets* nil
;;   "list of rockets to shoot.")

;; (defvar *missiles* nil
;;   "list of missiles to shoo.t")

(defparameter *tractor-beam-strength* 0.2)
(defparameter *booster-accel* 3)
(defparameter *missile-timeout* 200)
(defparameter *missile-cooldown* 70)
(defparameter *rocket-cooldown* 20)
(defparameter *rocket-shape* '(6 0 -6 -4 -6 4))
(defparameter *bullet-speed* 14)
(defparameter *missile-accel* 1.3)
(defparameter *collision-dampen* -0.1)
(defparameter *player-collision-transfer* 0.6)
(defparameter *bullet-collision-transfer* 0.1)
(defparameter *rocket-collision-transfer* 0.6)
(defparameter *missile-collision-transfer* 1.0)
(defparameter *collision-damage* 12)
(defparameter *bullet-damage* 2)
(defparameter *missile-damage* 10)
(defparameter *rocket-damage* 15)
(defparameter *bullet-radius* 3)
(defparameter *acceleration* 0.6)
(defparameter *angular-vel* 6)
(defparameter *gravity* 0.05)

(defvar *controls* nil)
(defvar *network-controls* nil
  "these are swapped in to read keystrokes in network
  mode. changes are sent to the server and the server tells the
  client what the state of *controls* is.")

(defun mirror-points (&rest points)
  (append points
	  (loop for i = (reverse points) then (cddr i)
	     while i
	     collect (second i)
	     collect (- (first i)))))

(defparameter *ships* 
  (list
   (make-ship :name "Crusty"
	      :guns (list 
		     ;; (make-gun :x 30 :y 0 :angle 0)
		     ;; (make-gun :x 30 :y 0 :angle 10)
		     ;; (make-gun :x 30 :y 0 :angle -10)
		     (make-gun :x -3 :y 20 :angle -5)
		     (make-gun :x -3 :y -20 :angle 5))
	      :specials '(:none :rocket :missile)
	      :special-pts (list (make-gun :x 30 :y 0 :angle 0)
				 (make-gun :x -13 :y 0 :angle 180))
	      :fire-rate 3
	      :shape `(30 3 30 -3 0 -7 -3 -20 -7 -20 -13 0 -7 20 -3 20 0 7)
	      :max-health 100
	      :max-ammo 300
	      :max-special '((:rocket 20)
			     (:missile 20))
	      :speed 8
	      :collision-balloons (list (make-balloon :x 0 :y 0 :radius 13)
					(make-balloon :x 24 :y 0 :radius 6)))

   (make-ship :name "Triangle"
	      :guns (list 
		     (make-gun :x 15 :y 0 :angle 0))
	      :specials '(:none :rocket :missile :booster :turret :tractor)
	      :special-pts (list (make-gun :x 15 :y 0 :angle 0)
				 (make-gun :x -5 :y 0 :angle 180))
	      :fire-rate 1
	      :shape `(15 0 -5 -5 -5 5)
	      :max-health 100
	      :max-ammo 300
	      :max-special '((:rocket 20)
			     (:missile 20)
			     (:booster 20)
			     (:turret 300)
			     (:tractor 2000))
	      :speed 8
	      :collision-balloons (list (make-balloon :x 3 :y 0 :radius 7)))
   (make-ship :name "Tie Fighter"
	      :shape `(-10 0 -3 5 -3 7 -17 12 -0 14
		       14 11 12 5 10 9 3 7 3 5 10 0
		       3 -5 3 -7 10 -9 12 -5 14 -11 0 -14
		       -17 -12 -3 -7 -3 -5)
	      :guns (list (make-gun :x 10 :y 3 :angle 0)
			  (make-gun :x 10 :y -3 :angle 0))
	      :special-pts (list (make-gun :x 10 :y 0 :angle 0)
				 (make-gun :x -10 :y 0 :angle 180))
	      :specials '(:none :rocket :missile)
	      :fire-rate 3
	      :max-health 100
	      :max-special '((:rocket 20)
			     (:missile 20))
	      :max-ammo 300
	      :speed 9
	      :collision-balloons (list (make-balloon :x 0 :y 0 :radius 15)))
   (make-ship :name "Scorpion"
	      :shape (mirror-points 15 0 12 3 16 14 19 13 16 17 19 17 10 23 10 6 -5 3 -7 7 -10 5 -9 0)
	      :guns (list (make-gun :x 19 :y 17 :angle -5)
			  (make-gun :x 19 :y -17 :angle 5)
			  (make-gun :x 15 :y 0 :angle 0))
	      :special-pts (list (make-gun :x 19 :y 17 :angle -5)
				 (make-gun :x 19 :y -17 :angle 5)
				 (make-gun :x 15 :y 0 :angle 0))
	      :specials '(:none :rocket :missile)
	      :fire-rate 3
	      :max-health 200
	      :max-special '((:rocket 20)
			     (:missile 20))
	      :max-ammo 500
	      :speed 6
	      :collision-balloons (list (make-balloon :x 2 :y 0 :radius 12)
					(make-balloon :x 13 :y 15 :radius 5)
					(make-balloon :x 13 :y -15 :radius 5)))
   (make-ship :name "Evader"
	      :shape (mirror-points 25 0 20 7 -5 15 -5 8 -10 8 -5 0)
	      :guns (list (make-gun :x -10 :y 8 :angle 180)
			  (make-gun :x -10 :y -8 :angle 180))
	      :special-pts (list (make-gun :x -10 :y 0 :angle 180)
				 (make-gun :x 25 :y 0 :angle 0))
	      :specials '(:none :rocket :missile)
	      :fire-rate 4
	      :max-health 200
	      :max-special '((:rocket 20)
			     (:missile 20))
	      :max-ammo 500
	      :speed 6
	      :collision-balloons (list (make-balloon :x 20 :y 0 :radius 6)
					(make-balloon :x 0 :y 0 :radius 13)))
))

(defun max-special (player)
  (or (second (assoc (player-special-type player) (ship-max-special (player-ship player))))
      1))

(defun d2r (angle)
  (* (/ angle 180) pi))

(defun r2d (angle)
  (/ (* angle 180) pi))

(defun color (r g b &optional a)
  (sdl:color :r r :g g :b b :a a))

(defun gray (intensity &optional alpha)
  (color intensity intensity intensity alpha))

(defun make-default-map ()
  (let ((width 1400)
	(height 1000)
	(gutter 100)
	(wall 20)
	(corner 200)
	(side 100)
	(i-corner 100))
    (make-game-map :pieces (list 
			    ;; center piece
			    (make-circle-piece :x (truncate width 2) :y (truncate height 2) :radius 70)
			    ;; outter wall
			    (make-rect-piece :x 0 :y 0 :width wall :height height)
			    (make-rect-piece :x (- width wall) :y 0 :width wall :height height)
			    (make-rect-piece :x 0 :y 0 :width width :height wall)
			    (make-rect-piece :x 0 :y (- height wall) :width width :height wall)
			    ;; tl corner
			    (make-rect-piece :x (+ wall gutter) :y (+ wall gutter) :width (+ corner side) :height wall)
			    (make-rect-piece :x (+ wall gutter) :y (+ wall gutter wall) :width wall :height (+ corner side))
			    (make-rect-piece :x (+ wall gutter wall i-corner) :y (+ wall gutter wall corner) :width i-corner :height wall)
			    (make-rect-piece :x (+ wall gutter corner) :y (+ wall gutter wall wall i-corner) :width wall :height i-corner)
			    ;; bl corner
			    (make-rect-piece :x (+ wall gutter) :y (- height wall gutter corner side) :width wall :height (+ corner side))
			    (make-rect-piece :x (+ wall gutter wall) :y (- height wall gutter wall) :width (+ corner side) :height wall)
			    (make-rect-piece :x (+ wall gutter wall i-corner wall) :y (- height wall gutter wall corner) :width i-corner :height wall)
			    (make-rect-piece :x (+ wall gutter wall corner) :y (- height wall gutter wall corner) :width wall :height i-corner)
			    ;; tr corner
			    (make-rect-piece :x (- width wall gutter corner side) :y (+ wall gutter) :width (+ corner side) :height wall)
			    (make-rect-piece :x (- width wall gutter wall) :y (+ wall gutter wall) :width wall :height (+ corner side))
			    (make-rect-piece :x (- width wall gutter wall corner) :y (+ wall gutter wall corner) :width i-corner :height wall)
			    (make-rect-piece :x (- width wall gutter corner wall) :y (+ wall gutter wall i-corner wall) :width wall :height i-corner)
			    ;; br corner
			    (make-rect-piece :x (- width wall gutter corner side) :y (- height wall gutter)  :width (+ corner side) :height wall)
			    (make-rect-piece :x (- width wall gutter wall) :y (- height wall gutter corner side) :width wall :height (+ corner side))
			    (make-rect-piece :x (- width wall gutter wall corner) :y (- height wall gutter wall corner) :width i-corner :height wall)
			    (make-rect-piece :x (- width wall gutter wall corner) :y (- height wall gutter wall corner) :width wall :height i-corner)
			    )
		   :starts '((200 200) (1200 200) (200 800) (1200 800)))))

(defun rotate-point (x y angle)
  (let ((angle (d2r angle)))
    (sdl:point :x (- (* (cos angle) x) (* (sin angle) y))
	       :y (+ (* (sin angle) x) (* (cos angle) y)))))
  
(defun rotate-scale-ofs-points (pts angle scale ofsx ofsy)
  "angle is in radians."
  (loop for i = pts then (cddr i)
       for x = (first i)
       for y = (second i)
       while i
       collect (sdl:point :x (truncate (* scale (+ ofsx (- (* (cos angle) x) (* (sin angle) y)))))
			  :y (truncate (* scale (+ ofsy (+ (* (sin angle) x) (* (cos angle) y))))))))

(defun rotate-scale-points (pts angle scale ofsx ofsy)
  "angle is in radians."
  (loop for i = pts then (cddr i)
       for x = (first i)
       for y = (second i)
       while i
       collect (sdl:point :x (truncate (+ ofsx (* scale (- (* (cos angle) x) (* (sin angle) y)))))
			  :y (truncate (+ ofsy (* scale (+ (* (sin angle) x) (* (cos angle) y))))))))

;; set it to black
;; (defun clear-pixmap (pixmap)
;;   (xlib:draw-rectangle pixmap *clear-gc* 0 0 640 455 t))

;; (defun blit-pixmap (pixmap window)
;;   (xlib:copy-area pixmap *copy-gc* 0 0 640 455 window 0 0))

(defgeneric draw-piece (piece x y scale))

(defmethod draw-piece ((piece rect-piece) x y scale)
  (sdl:draw-box-* (truncate (* (- (rect-piece-x piece) x) scale))
		  (truncate (* (- (rect-piece-y piece) y) scale))
		  (truncate (* (rect-piece-width piece) scale))
		  (truncate (* (rect-piece-height piece) scale))
		  :color (color 178 178 0)))

(defmethod draw-piece ((piece circle-piece) x y scale)
  (let* ((fradius (* (circle-piece-radius piece) scale)))
    (sdl-gfx:draw-filled-circle-*
     (truncate (* (- (circle-piece-x piece) x) scale))
     (truncate (* (- (circle-piece-y piece) y) scale))
     (truncate fradius)
     :color (color 178 178 0))))

(defun draw-balloon (balloon x y angle scale)
  (let* ((fradius (* (balloon-radius balloon) scale))
	 rx ry)
    (multiple-value-setq (rx ry) (rotate-point (balloon-x balloon) (balloon-y balloon) angle))
    (sdl-gfx:draw-filled-circle-*
     (truncate (* (+ rx x) scale))
     (truncate (* (+ ry y) scale))
     fradius
     :color (color 178 178 0))))

(defun draw-ship-at (x y angle scale ship color)
  (let* ((pts (rotate-scale-points (ship-shape ship) (d2r angle) scale x y)))
    (sdl-gfx:draw-filled-polygon pts :color color)
    (sdl-gfx:draw-aa-polygon pts :color color)))

(defun draw-ship (x y angle scale ship color)
  "angle in degrees"
  (let* ((pts (rotate-scale-ofs-points (ship-shape ship) (d2r angle) scale x y)))
    (sdl-gfx:draw-filled-polygon pts :color color)
    (sdl-gfx:draw-aa-polygon pts :color color)

;;     (loop for i in (ship-collision-balloons ship) do
;; 	 (draw-balloon i x y angle scale drawable))
    ))

(defun draw-map (map ofsx ofsy scale)
  ;; if we're close enough, draw the map lines to avoid disorientation
  (let* ((intensity 150)
         (idx (- intensity (min (truncate (- (min (/ (game-area-width) scale)
                                                  (/ (game-area-height) scale))
                                             *map-max-zoom*)) intensity))))
    (when (> idx 0)
      (let ((color (color idx
                          (truncate 10 (/ 255 idx))
                          idx)))
      (loop for i from (* (- (mod ofsx 20)) scale) to (game-area-width) by (* 20 scale) do
	   (sdl:draw-line-* (truncate i) 0 (truncate i) (game-area-height) :color color))
      (loop for i from (* (- (mod ofsy 20)) scale) to (game-area-height) by (* 20 scale) do
	   (sdl:draw-line-* 0 (truncate i) (game-area-width) (truncate i) :color color)))))
  (dolist (i (map-pieces map))
    (draw-piece i ofsx ofsy scale)))

(defun point-circle-collision (x y cx cy radius)
  (< (sqrt (+ (expt (- x cx) 2)
	      (expt (- y cy) 2)))
     radius))

(defun circle-circle-collision (cx1 cy1 radius1 cx2 cy2 radius2)
  (< (sqrt (+ (expt (- cx1 cx2) 2)
	      (expt (- cy1 cy2) 2)))
     (+ radius1 radius2)))

(defgeneric point-piece-collision (x y piece))

(defmethod point-piece-collision (x y (piece circle-piece))
  (point-circle-collision x y (circle-piece-x piece) (circle-piece-y piece) (circle-piece-radius piece)))

(defmethod point-piece-collision (x y (piece rect-piece))
  (not (or (< x (rect-piece-x piece))
	   (> x (+ (rect-piece-x piece) (rect-piece-width piece)))
	   (< y (rect-piece-y piece))
	   (> y (+ (rect-piece-y piece) (rect-piece-height piece))))))

(defun bullet-map-collision (bullet map)
  (find-if (lambda (p)
             (point-piece-collision (bullet-x bullet) (bullet-y bullet) p))
           (map-pieces map)))

(defun player-piece-collision (pts piece)
  (loop for i = pts then (cddr i)
     while i
     when (point-piece-collision (first i) (second i) piece)
     return t))

(defun player-map-collision (player map)
  (let* ((angle (d2r (player-angle player)))
	 (pts (loop for i = (ship-shape (player-ship player)) then (cddr i)
		 while i
		 collect (+ (player-x player) (- (* (cos angle) (first i)) (* (sin angle) (second i))))
		 collect  (+ (player-y player) (+ (* (sin angle) (first i)) (* (cos angle) (second i)))))))
    (not (loop for i in (map-pieces map)
	    never (player-piece-collision pts i)))))

(defun rotate-ofs-balloons (x y angle balloons)
  (let ((angle (d2r angle)))
    (loop for i in balloons
       collect (+ x (- (* (cos angle) (balloon-x i)) (* (sin angle) (balloon-y i))))
       collect (+ y (+ (* (sin angle) (balloon-x i)) (* (cos angle) (balloon-y i))))
       collect (balloon-radius i))))

(defgeneric do-projectile-damage (player bullet))

(defmethod do-projectile-damage (player (bullet bullet))
  (push-player player
	       (* *bullet-collision-transfer* (bullet-vx bullet))
	       (* *bullet-collision-transfer* (bullet-vy bullet)))
  ;; damage the player after so the velocity change doesn't affect them
  ;; if they die and reset.
  (damage-player player *bullet-damage*))

(defmethod do-projectile-damage (player (bullet rocket))
  ;;(setf (player-special-active (rocket-owner bullet)) nil)
  (push-player player
	       (* *rocket-collision-transfer* (bullet-vx bullet))
	       (* *rocket-collision-transfer* (bullet-vy bullet)))
  ;; damage the player after so the velocity change doesn't affect them
  ;; if they die and reset.
  (damage-player player *rocket-damage*))

(defmethod do-projectile-damage (player (bullet missile))
  (push-player player
	       (* *missile-collision-transfer* (bullet-vx bullet))
	       (* *missile-collision-transfer* (bullet-vy bullet)))
  ;; damage the player after so the velocity change doesn't affect them
  ;; if they die and reset.
  (damage-player player *missile-damage*))

(defun player-bullets-collision (bullets player)
  (let ((balloons (rotate-ofs-balloons (player-x player) (player-y player) (player-angle player) 
				       (ship-collision-balloons (player-ship player)))))
    (delete-if (lambda (i)
		 (when (not (loop for b = balloons then (cdddr b)
			       while b
			       never (point-circle-collision (bullet-x i) (bullet-y i) (first b) (second b) (third b))))
		   ;; the damage side effect
		   (do-projectile-damage player i)
		   t))
	       bullets)))

;; (defun player-player-collision (player1-balloons player2-balloons)
;;   (loop for i = player1-balloons then (cdddr i)
;;      while i
;;      always (not (loop for j = player2-balloons then (cdddr j)
;; 		    while j
;; 		    never (circle-circle-collision (first i) (second i) (third i)
;; 						   (first j) (second j) (third j))))))

(defun update-for-player-map-collision (player map angular-direction)
  "quick search for a state where the player and map don't
collide. Do it 5 times and set the player to the last
non-colliding position. Return T if a collision occurred."
  (when (player-map-collision player map)
    ;; undo the move
    (decf (player-x player) (player-vx player))
    (decf (player-y player) (player-vy player))
    (decf (player-angle player) (* *angular-vel* angular-direction))
    (let ((x (player-x player))
	  (y (player-y player))
	  (angle (player-angle player))
	  (vx (player-vx player))
	  (vy (player-vy player))
	  (va (* *angular-vel* angular-direction)))
      (loop for i from 1 to 3
	 do (setf vx (/ vx 2)
		  vy (/ vy 2)
		  va (/ va 2))
	 if (player-map-collision player map) do
	 (decf (player-x player) vx)
	 (decf (player-y player) vy)
	 (decf (player-angle player) va)
	 else do
	 ;; the last non-collision point
	 (setf x (player-x player)
	       y (player-y player)
	       angle (player-angle player))
	 (incf (player-x player) vx)
	 (incf (player-y player) vy)
	 (incf (player-angle player) va))
      ;; restore the player to the last non-collision point. this
      ;; player is now solidified.
      (setf (player-x player) x
	    (player-y player) y
	    (player-angle player) angle
	    (player-solidified player) t)
      t)))

(defun balloons-balloons-collision (balloons1 balloons2)
  (not (loop for i = balloons1 then (cdddr i)
	  while i
	  always (loop for j = balloons2 then (cdddr j)
		    while j
		    never (circle-circle-collision (first i) (second i) (third i)
						   (first j) (second j) (third j))))))

(defun move-players-for-collision (player1 player2)
  "adjust players' positions so they dont collide."
  (let ((lg1 (make-gun :x (player-last-x player1)
		       :y (player-last-y player1)
		       :angle (player-last-angle player1)))
	(lg2 (make-gun :x (player-last-x player2)
		       :y (player-last-y player2)
		       :angle (player-last-angle player2)))
	(dx1 (- (player-x player1) (player-last-x player1)))
	(dy1 (- (player-y player1) (player-last-y player1)))
	(da1 (- (player-angle player1) (player-last-angle player1)))
	(dx2 (- (player-x player2) (player-last-x player2)))
	(dy2 (- (player-y player2) (player-last-y player2)))
	(da2 (- (player-angle player2) (player-last-angle player2))))
    ;; restore the players to their last known non-colliding positions
    (unless (player-solidified player1)
      (setf (player-x player1) (player-last-x player1)
	    (player-y player1) (player-last-y player1)
	    (player-angle player1) (player-last-angle player1)))
    (unless (player-solidified player2)
      (setf (player-x player2) (player-last-x player2)
	    (player-y player2) (player-last-y player2)
	    (player-angle player2) (player-last-angle player2)))
    (loop for i from 1 to 5 do
       ;; halve the change
	 (setf dx1 (/ dx1 2)
	       dy1 (/ dy1 2)
	       da1 (/ da1 2)
	       dx2 (/ dx2 2)
	       dy2 (/ dy2 2)
	       da2 (/ da2 2))
	 (if (player-player-collide-p player1 player2)
	     (progn
	       (unless (player-solidified player1)
		 (decf (player-x player1) dx1)
		 (decf (player-y player1) dy1)
		 (decf (player-angle player1) da1))
	       (unless (player-solidified player2)
		 (decf (player-x player2) dx2)
		 (decf (player-y player2) dy2)
		 (decf (player-angle player2) da2)))
	     (progn
	       (unless (player-solidified player1)
		 (incf (player-x player1) dx1)
		 (incf (player-y player1) dy1)
		 (incf (player-angle player1) da1)
		 (setf (gun-x lg1) (player-x player1)
		       (gun-y lg1) (player-y player1)
		       (gun-angle lg1) (player-angle player1)))
	       (unless (player-solidified player2)
		 (incf (player-x player2) dx2)
		 (incf (player-y player2) dy2)
		 (incf (player-angle player2) da2)
		 (setf (gun-x lg2) (player-x player2)
		       (gun-y lg2) (player-y player2)
		       (gun-angle lg2) (player-angle player2))))))
    ;; give the players their last non-colliding positions
    (unless (player-solidified player1)
      (setf (player-x player1) (gun-x lg1)
	    (player-y player1) (gun-y lg1)
	    (player-angle player1) (gun-angle lg1)))
    (unless (player-solidified player2)
      (setf (player-x player2) (gun-x lg2)
	    (player-y player2) (gun-y lg2)
	    (player-angle player2) (gun-angle lg2)))
    ;; these players are now solidified
    (setf (player-solidified player1) t
	  (player-solidified player2) t)))

(defun player-player-collide-p (p1 p2)
  (let ((b1 (rotate-ofs-balloons (player-x p1) (player-y p1) (player-angle p1)
				 (ship-collision-balloons (player-ship p1))))
	(b2 (rotate-ofs-balloons (player-x p2) (player-y p2) (player-angle p2)
				 (ship-collision-balloons (player-ship p2)))))
    (balloons-balloons-collision b1 b2)))
  
(defun update-for-player-player-collision (players)
  (let ((balloons (loop for i in players 
		     collect (rotate-ofs-balloons (player-x i) (player-y i) (player-angle i) 
						  (ship-collision-balloons (player-ship i))))))
    ;; loop by index so the balloons can be updated
    (loop 
       for i on players
       for j from 0 do
       (loop
	  for k in (cdr i)
	  for l from (1+ j) do
	  (when (balloons-balloons-collision (nth j balloons) (nth l balloons))
	    ;; adjust them so they dont collide
	    (move-players-for-collision (car i) k)
	    ;; their balloons are now out of date, so recalculate them
	    (setf (nth j balloons) (rotate-ofs-balloons (player-x (car i)) (player-y (car i)) (player-angle (car i))
							(ship-collision-balloons (player-ship (car i))))
		  (nth l balloons) (rotate-ofs-balloons (player-x k) (player-y k) (player-angle k) 
							(ship-collision-balloons (player-ship k))))
	    ;; swap their velocities
	    (psetf (player-vx (car i)) (* *player-collision-transfer* (player-vx k))
		   (player-vy (car i)) (* *player-collision-transfer* (player-vy k))
		   (player-vx k) (* *player-collision-transfer* (player-vx (car i)))
		   (player-vy k) (* *player-collision-transfer* (player-vy (car i)))))))))

(defun explode-projectile (b deg magnitude)
  "Send a spray of bullets in all directions"
  (loop for angle from 0 below 360 by deg do
       (shoot-bullet :x (bullet-x b)
		     :y (bullet-y b)
		     :vx (* (cos (d2r angle)) magnitude)
		     :vy (* (sin (d2r angle)) magnitude)
		     :color (bullet-color b))))

;; (defun angle-between-vectors (x1 y1 x2 y2)
;;   ;; dot prod / |A| |B| = cos (angle)
;;   (/ (+ (* x1 x2) (* y1 y2))
;;      (* (sqrt (+ (* x1 x1) (* y1 y1)))
;; 	(sqrt (+ (* x2 x2) (* y2 y2))))))

(defun pointing-angle (x1 y1 x2 y2)
  (let* ((nx (- x2 x1))
	 (ny (- y2 y1))
	 (a (atan (or (ignore-errors (/ ny nx)) 0))))
    (when (< nx 0)
      (incf a pi))
    a))

(defun closest-player (x y players)
  (loop for i in players
     with min = nil
     with min-player = nil
     do (let ((dist (sqrt (+ (expt (- x (player-x i)) 2) (expt (- y (player-y i)) 2)))))
	  (when (or (null min)
		    (> min dist))
	    (setf min dist 
		  min-player i)))
     finally (return min-player)))

(defun projection-length (x1 y1 vx vy)
  "Return the length of the project of vx vy onto x1 y1"
  (or (ignore-errors (/ (+ (* x1 vx) (* y1 vy))
			(sqrt (+ (* x1 x1) (* y1 y1)))))
      0))

(defun maxout (x y max)
  (if (> (sqrt (+ (* x x) (* y y))) max)
      (let ((a (pointing-angle 0 0 x y)))
	(values (* (cos a) max)
		(* (sin a) max)))
      (values x y)))

(defun update-missiles (players map)
  (dolist (i (state-missiles *state*))
    (when (< (missile-timer i) 190)
      (let* ((closest (closest-player (bullet-x i) (bullet-y i) players))
	     (a (pointing-angle (bullet-x i) (bullet-y i) (player-x closest) (player-y closest))))
	(incf (missile-vx i) (* (cos a) *missile-accel*))
	(incf (missile-vy i) (* (sin a) *missile-accel*))
	(multiple-value-bind (mx my) (maxout (missile-vx i) (missile-vy i) *bullet-speed*)
	  (setf (missile-vx i) mx
		(missile-vy i) my))))
    (incf (missile-x i) (missile-vx i))
    (incf (missile-y i) (missile-vy i))
    (setf (missile-angle i) (/ (* 180 (pointing-angle 0 0 (missile-vx i) (missile-vy i))) pi))
    (decf (missile-timer i)))
  (setf (state-missiles *state*) (delete-if (lambda (b)
					      (or (bullet-map-collision b map)
						  (when (<= (missile-timer b) 0)
						    (explode-projectile b 60 10)
						    t))) (state-missiles *state*)))
  (loop for i in players do
       (setf (state-missiles *state*) (player-bullets-collision (state-missiles *state*) i))))

(defun update-rockets (players map)
  (dolist (i (state-rockets *state*))
    (incf (rocket-vy i) *gravity*)
    (incf (rocket-x i) (rocket-vx i))
    (incf (rocket-y i) (rocket-vy i)))
  (setf (state-rockets *state*) (delete-if (lambda (b)
					     (cond ((bullet-map-collision b map)
						    ;;(setf (player-special-active (bullet-owner b)) nil)
						    t)
						   ((not (controls-special (player-controls (rocket-owner b))))
						    (explode-projectile b 15 5)
						    ;;(setf (player-special-active (bullet-owner b)) nil)
						    t)
						   (t nil))) (state-rockets *state*)))
  (loop for i in players do
       (setf (state-rockets *state*) (player-bullets-collision (state-rockets *state*) i))))

(defun update-bullets (players map)
  (dolist (i (state-bullets *state*))
    (incf (bullet-vy i) *gravity*)
    (incf (bullet-x i) (bullet-vx i))
    (incf (bullet-y i) (bullet-vy i)))
  (setf (state-bullets *state*) (delete-if (lambda (b)
					     (bullet-map-collision b map)) (state-bullets *state*)))
  (loop for i in players do
       (setf (state-bullets *state*) (player-bullets-collision (state-bullets *state*) i))))

(defun draw-bullets (x y scale)
  (let* ((fradius (* 3 scale)))
    (dolist (i (state-bullets *state*))
      (sdl-gfx:draw-filled-circle-*
		     (truncate (* (- (bullet-x i) x) scale))
		     (truncate (* (- (bullet-y i) y) scale))
		     (truncate fradius) :color (bullet-color i)))))

(defun draw-projectiles (projectiles x y scale)
  (dolist (i projectiles)
    (sdl-gfx:draw-filled-polygon (rotate-scale-ofs-points *rocket-shape* (d2r (projectile-angle i)) scale (- (bullet-x i) x) (- (bullet-y i) y))
                                 :color (bullet-color i))))

(defun draw-rockets (x y scale)
  (draw-projectiles (state-rockets *state*) x y scale))

(defun draw-missiles (x y scale)
  (draw-projectiles (state-missiles *state*) x y scale))

(defun shoot-bullet (&key x y vx vy color)
  (push
   (make-bullet  :x x :y y :vx vx :vy vy :color color)
   (state-bullets *state*)))

(defun shoot-rocket (player)
  (when (and (not (player-special-active player))
	     (<= (player-special-cooldown player) 0)
	     (> (player-special player) 0))
    (let ((angle (+ (gun-angle (player-special-gun player)) (player-angle player)))
          (pt (rotate-point (gun-x (player-special-gun player)) (gun-y (player-special-gun player)) (player-angle player))))
      (push
       (make-rocket :owner player 
                    :angle angle
                    :x (+ (sdl:x pt) (player-x player))
                    :y (+ (sdl:y pt) (player-y player))
                    :vx (+ (player-vx player)
                           (* (cos (d2r angle)) *bullet-speed*))
                    :vy (+ (player-vy player)
                           (* (sin (d2r angle)) *bullet-speed*))
                    :color (player-color player))
       (state-rockets *state*))
      (decf (player-special player))
      (setf (player-special-active player) t
	    (player-special-cooldown player) *rocket-cooldown*))))

(defun shoot-tractor (player players)
  (when (> (player-special player) 0)
    (let ((pt (rotate-point (gun-x (player-special-gun player)) (gun-y (player-special-gun player)) (player-angle player))))
      (incf (sdl:x pt) (player-x player))
      (incf (sdl:y pt) (player-y player))
      (let* ((closest (closest-player (sdl:x pt) (sdl:y pt) (remove player players)))
	     (a (pointing-angle (player-x closest) (player-y closest) (sdl:x pt) (sdl:y pt))))
	(incf (player-vx closest) (* (cos a) *tractor-beam-strength*))
	(incf (player-vy closest) (* (sin a) *tractor-beam-strength*)))
      (decf (player-special player)))))

(defun shoot-turret (player players)
  (when (> (player-special player) 0)
    (let ((pt (rotate-point (gun-x (player-special-gun player)) (gun-y (player-special-gun player)) (player-angle player))))
      (incf (sdl:x pt) (player-x player))
      (incf (sdl:y pt) (player-y player))
      (let* ((closest (closest-player (sdl:x pt) (sdl:y pt) (remove player players)))
	     (a (pointing-angle (sdl:x pt) (sdl:y pt) (player-x closest) (player-y closest))))
	;; FIXME: if the closest player is behind the turret, the turret will just shoot its owner
	(shoot-bullet :x (sdl:x pt) :y (sdl:y pt)
		      :vx (* (cos a) *bullet-speed*)
		      :vy (* (sin a) *bullet-speed*)
		      :color (player-color player))
	(decf (player-special player))))))

(defun shoot-booster (player)
  (when (> (player-special player) 0)
    (let ((a (+ (player-angle player) (gun-angle (player-special-gun player)))))
      (decf (player-vx player) (* (cos (d2r a)) *booster-accel*))
      (decf (player-vy player) (* (sin (d2r a)) *booster-accel*))
      (decf (player-special player)))))

(defun shoot-missile (player)
  (when (and (<= (player-special-cooldown player) 0)
	     (> (player-special player) 0))
    (let ((angle (+ (gun-angle (player-special-gun player)) (player-angle player)))
          (pt (rotate-point (gun-x (player-special-gun player)) (gun-y (player-special-gun player)) (player-angle player))))
      (push
       (make-missile :timer *missile-timeout*
                     :angle angle
                     :x (+ (sdl:x pt) (player-x player))
                     :y (+ (sdl:y pt) (player-y player))
                     :vx (+ (player-vx player)
                            (* (cos (d2r angle)) *bullet-speed*))
                     :vy (+ (player-vy player)
                            (* (sin (d2r angle)) *bullet-speed*))
                     :color (player-color player))
       (state-missiles *state*))
      (decf (player-special player))
      (setf (player-special-cooldown player) *missile-cooldown*))))

(defun player-shoot (player)
  (when (and (> (player-ammo player) 0)
	     (zerop (player-cool-down player)))
    (setf (player-cool-down player) (ship-fire-rate (player-ship player)))
    (dolist (i (ship-guns (player-ship player)))
      (when (> (player-ammo player) 0)
	(decf (player-ammo player))
	(setf (state-status-bar-needs-updating *state*) t)
	(let ((pt (rotate-point (gun-x i) (gun-y i) (player-angle player))))
	  (shoot-bullet :x (+ (player-x player) (sdl:x pt))
			:y (+ (player-y player) (sdl:y pt))
			:vx (+ (player-vx player)
			       (* (cos (d2r (+ (gun-angle i) (player-angle player)))) *bullet-speed*))
			:vy (+ (player-vy player)
			       (* (sin (d2r (+ (gun-angle i) (player-angle player)))) *bullet-speed*))
			:color (player-color player)))))))

(defun push-player (player vx vy)
  (incf (player-vx player) vx)
  (incf (player-vy player) vy))

(defun reset-player (player)
  (decf (player-lives player))
  (when (> (player-lives player) 0)
    (setf (player-health player) (ship-max-health (player-ship player))
	  (player-ammo player) (ship-max-ammo (player-ship player))
	  (player-special player) (max-special player)
	  (player-x player) (player-start-x player)
	  (player-y player) (player-start-y player)
	  (player-angle player) -90)))

(defun damage-player (player amt)
  (decf (player-health player) amt)
  (when (< (player-health player) 0)
    (reset-player player))
  (setf (state-status-bar-needs-updating *state*) t))

(defun mag-min (a b)
  (if (< (abs a) (abs b))
	 a b))

(defun update-player (player map players)
  (let ((angular-direction 0))
    (when (controls-left (player-controls player)) 
      (decf (player-angle player) *angular-vel*)
      (decf angular-direction 1))
    (when (controls-right (player-controls player))
      (incf (player-angle player) *angular-vel*)
      (incf angular-direction 1))
    (when (controls-forward (player-controls player))
      (let* ((vx (cos (d2r (player-angle player))))
	     (vy (sin (d2r (player-angle player))))
	     (len (projection-length vx vy (player-vx player) (player-vy player)))
	     (max (ship-speed (player-ship player))))
	;; a cheap hack to keep the player maxed out unless they're
	;; going "really fast" then only going backwards works.
	(if (< (abs len) (+ max 5))
	    (progn
	      (incf (player-vx player) (* *acceleration* vx))
	      (incf (player-vy player) (* *acceleration* vy))
	      (multiple-value-bind (mx my) (maxout (player-vx player) (player-vy player) max)
		(setf (player-vx player) mx)
		(setf (player-vy player) my)
		(when (< len 0)
		  (incf (player-vx player) (* *acceleration* vx))
		  (incf (player-vy player) (* *acceleration* vy))))))
;; 	(let ((speed (sqrt (+ (* (player-vx player) (player-vx player))
;; 			      (* (player-vy player) (player-vy player))))))
;; 	(when (> speed max)
;; 	  (format t "~,2f ~,2f ~,2f~%" speed max (- speed max)))
;; 	)
      ))
    (unless (zerop (player-cool-down player))
      (decf (player-cool-down player)))
    (when (controls-shoot (player-controls player))
      (player-shoot player))
    (when (> (player-special-cooldown player) 0)
      (decf (player-special-cooldown player)))
    ;; if the rocket hits a player the player must release the key
    ;; before firing another
    (when (and (player-special-active player)
	       (not (controls-special (player-controls player))))
      (setf (player-special-active player) nil))
    (when (controls-special (player-controls player))
      (ecase (player-special-type player)
	(:missile (shoot-missile player))
	(:rocket (shoot-rocket player))
	(:none)
	(:tractor (shoot-tractor player players))
	(:turret (shoot-turret player players))
	(:booster (shoot-booster player)))
      (setf (state-status-bar-needs-updating *state*) t))
    (incf (player-vy player) *gravity*)
    (incf (player-x player) (player-vx player))
    (incf (player-y player) (player-vy player))
    (when (update-for-player-map-collision player map angular-direction)
      ;; this is really cheap.
      (setf (player-vx player) (* (player-vx player) *collision-dampen*)
	    (player-vy player) (* (player-vy player) *collision-dampen*))
      ;; damage the player after so the velocity change doesn't affect them
      ;; if they die and reset.
      (damage-player player *collision-damage*))))

(defun draw-status-bar (player x y drawable)
  ;; bg
  (sdl:draw-box-* x y 150 25 :surface drawable :color (player-color player))
  ;; player color
  ;;(xlib:draw-rectangle drawable (player-gc player) (+ x 5) (+ y 5) 30 15 t)
  (when (> (player-lives player) 0)
    (let ((txt (format nil "~d" (player-lives player))))
      (sdl:draw-string-blended-* txt (- (+ x 15) (truncate (sdl:get-font-size txt :size :w :font *lives-font*) 2)) (+ y 5)
                                 :surface drawable :font *lives-font* :color (gray 0)))
    ;; stats
    (sdl:draw-box-* (+ x 50) (+ y 3) 90 5 :surface drawable :color (gray 50))
    (sdl:draw-box-* (+ x 50) (+ y 3) (truncate (* 90 (/ (player-health player) (ship-max-health (player-ship player))))) 5 :surface drawable :color (gray 200))
    (sdl:draw-box-* (+ x 50) (+ y 10) 90 5 :surface drawable :color (gray 50))
    (sdl:draw-box-* (+ x 50) (+ y 10) (truncate (* 90 (/ (player-ammo player) (ship-max-ammo (player-ship player))))) 5 :surface drawable :color (gray 200))
    (sdl:draw-box-* (+ x 50) (+ y 17) 90 5  :surface drawable :color (gray 50))
    (sdl:draw-box-* (+ x 50) (+ y 17) (truncate (* 90 (/ (player-special player) (max-special player)))) 5  :surface drawable :color (gray 200))))

(defun draw-status-bars (players x y drawable)
  (loop
     for i in players
     for rx from 0 by 155 do
       (draw-status-bar i (+ x rx) y drawable)))

(defun update-status-bar (players)
  (when (state-status-bar-needs-updating *state*)
    (sdl:clear-display (sdl:color) :surface *status-buffer*)
    (draw-status-bars players 0 0 *status-buffer*)
    (setf (state-status-bar-needs-updating *state*) nil)))

(defun blit-status-bar ()
  (sdl:draw-surface-at-* *status-buffer* 0 (1+ (game-area-height))))

(defun build-player (&key controls ship special special-pt start-x start-y color)
  (make-player :controls controls
	       :ship ship
	       :ammo (ship-max-ammo ship) :health (ship-max-health ship) :special (or (second (assoc special (ship-max-special ship))) 1)
	       :special-type special
	       :special-gun special-pt
	       :start-x start-x :start-y start-y
	       :x start-x :y start-y
	       :angle -90 :vx 0 :vy 0
	       :lives 5
	       :color color))

(defun reset-selection (selection)
  (setf (ship-selection-ship selection) 0
	(ship-selection-special selection) nil
	(ship-selection-special-pt selection) nil
	(ship-selection-confirm selection) nil
	;; (ship-selection-wait selection) nil
	))

(defun draw-text (x y txt &optional (color *text-color*))
  (sdl:draw-string-blended-* txt (truncate (- x (/ (sdl:get-font-size txt :size :w :font *font*) 2))) (truncate y) :color color :font *font*))

(defun draw-special (special x y)
  (let ((txt (ecase special
	       (:none "No special")
	       (:missile "Missile")
	       (:rocket "Rocket")
	       (:tractor "Tractor Beam")
	       (:turret "Turret")
	       (:booster "Booster"))))
    (draw-text x y txt)))

(defun make-bubbles ()
  "make a cheap but neat looking background"
  (let ((s (sdl:create-surface (screen-width) (screen-height))))
    (sdl:clear-display (gray 100) :surface s)
    (dotimes (i 1000)
      (sdl-gfx:draw-filled-circle-* (- (random (screen-width)) 0) (- (random (screen-height)) 0) (random 50) :surface s :color (gray (+ 100 (random 50)) 128)))
    s))

(defun make-fire-bubbles (width height thickness &key (color (gray 255)))
  "make a cheap but neat looking background"
  (let ((s (sdl:create-surface width height)))
    (sdl:clear-display (gray 100) :surface s)
    (dotimes (i 1000)
      (sdl-gfx:draw-filled-circle-* (random (screen-width)) (random (screen-height)) (random 50) :surface s :color (color (+ 200 (random 50)) (+ 200 (random 50)) 0 128)))
    (sdl:draw-box-* 0 0 thickness height :color color :surface s)
    (sdl:draw-box-* (- width thickness) 0 thickness height :color color :surface s)
    s))

(defun dump-color (color)
  `(:color ,(sdl:r color) ,(sdl:g color) ,(sdl:b color)))

(defun restore-color (dump)
  (assert (eq (first dump) :color))
  (sdl:color :r (second dump)
             :g (third dump)
             :b (fourth dump)))

(defmacro defdump (class (&rest slots) &optional dump-body restore-body)
  (let ((p (gensym "P"))
        (i (gensym "I")))
    `(progn
       (defun ,(intern (format nil "~a-~a" :dump class)) (obj)
         (let ((new (loop with ,p = (,(intern (format nil "~a-~a" :make class)))
                       for ,i in ',slots
                       do (setf (slot-value ,p ,i) (slot-value obj ,i))
                       finally (return ,p))))
           ,dump-body
           new))
       (defun ,(intern (format nil "~a-~a" :restore class)) (obj dump)
         (loop for ,i in ',slots
            do (setf (slot-value obj ,i) (slot-value dump ,i)))
         ,restore-body))))

(defdump player
    (start-x start-y last-x last-y
             last-angle angle x y vx vy
             health ammo special special-cooldown special-active cool-down
             lives last-good))

(defdump bullet (x y vx vy)
  (setf (bullet-color new) (dump-color (bullet-color obj)))
  (setf (bullet-color obj) (restore-color (bullet-color dump))))

;; (defdump missile 
;;     x y vx vy angle target timer)

;; (defdump rocket
;;     x y vx vy angle owner)

(defun dump-slots (obj &rest slots)
  (let ((new (make-instance (type-of obj))))
    (loop for s in slots do
         (setf (slot-value new s) (slot-value obj s)))
    new))

(defun restore-slots (obj dump &rest slots)
  (loop for s in slots do
       (setf (slot-value obj s) (slot-value dump s))))

(defun dump-rocket (obj players)
  (let ((new (dump-slots obj 'x 'y 'vx 'vy 'angle)))
    (setf (rocket-owner new) (position (rocket-owner obj) players)
          (rocket-color new) (dump-color (rocket-color obj)))
    new))

(defun restore-rocket (obj dump players)
  (restore-slots obj dump)
  (setf (rocket-owner obj) (elt players (rocket-owner dump))
        (rocket-color obj) (restore-color (rocket-color dump))))

(defun dump-missile (obj players)
  (let ((new (dump-slots obj 'x 'y 'vx 'vy 'angle 'timer)))
    (setf (missile-target new) (position (missile-target obj) players)
          (missile-color new) (dump-color (missile-color obj)))
    new))

(defun restore-missile (obj dump players)
  (restore-slots obj dump 'x 'y 'vx 'vy 'angle 'timer)
  (setf (missile-target obj) (elt players (missile-target dump))
        (missile-color obj) (restore-color (missile-color dump))))

(defun dump-choose-state (state)
  (list :choose-stage state))

(defun restore-choose-state (packet)
  (assert (eq (first packet) :choose-stage))
  (second packet))

(defun choose-stage (&optional server)
  "Return a list of players. server could be a cl-server or an sv-server."
  (macrolet ((forward (slot list player)
	       `(setf (,slot ,player) (mod (1+ (,slot ,player)) (length ,list))))
	     (backward (slot list player)
	       `(progn
		  (decf (,slot ,player))
		  (when (< (,slot ,player) 0)
		    (setf (,slot ,player) (1- (length ,list)))))))
    (labels ((ships (player)
	       (declare (ignore player))
	       *ships*)
	     (specials (player)
	       (ship-specials (nth (ship-selection-ship player) *ships*)))
	     (special-pts (player)
	       (ship-special-pts (nth (ship-selection-ship player) *ships*))))
      (let* ((players (list (make-ship-selection :ship 0) nil nil nil))
             (angle 0)
             (colors (list *player1-color* *player2-color* *player3-color* *player4-color*))
             (width (truncate (screen-width) 2.5))
             (height (truncate (screen-height) 2.5))
             (padx (truncate (- (screen-width) (* width 2)) 3))
             (pady (truncate (- (screen-height) (* height 2)) 3))
             (scale (* 1.9 (/ (screen-width) width)))
             (xs `(,padx ,(+ width (* padx 2)) padx (+ width (* padx 2))))
             (ys `(,pady ,pady ,(+ height (* pady 2)) ,(+ height (* pady 2))))
             (start (get-internal-real-time))
             (bk-players nil)
             (bk-controls (make-empty-controls-array 4))
             (bubbles (make-bubbles)))
	(catch 'done
	  (loop
             ;;(sdl:clear-display (sdl:color))
             (sdl:draw-surface bubbles)
	     (setf angle (mod (+ angle 2) 360))
	     ;; depending on the server type we handle event
	     ;; processing differently.
	     (cond 
	       ((server-p server)
                ;; send the new choose state if anything changed
                (unless (equalp players bk-players)
		  (sv-send-packet server (dump-choose-state players))
                  (setf bk-players (mapcar (lambda (x) (when x (copy-structure x))) players)))
		;; anyone joined the game?
		(let ((client (sv-check-for-new-clients server)))
		  (when client
		    (setf (nth (controls-id (sv-client-controls client)) players) (make-ship-selection :ship 0))
                    ;; tell the joining player about everyone else
                    (sv-send-packet-to client (dump-choose-state players))))
		;; check for key events
                (with-sv-packets (server)
                  (:controls
                   (import-controls (sv-client-controls %client%) (second %packet%))))
		(unless (sv-server-dedicated-p server)
		  (process-events (vector (aref *controls* 0))))
		;; if someone disconnected then return their controls
		;; and zap their ship selection
		(setf (sv-server-clients server)
		      (remove-if (lambda (i)
				   (when (sv-client-disconnected-p i)
				     (push (sv-client-controls i) (sv-server-free-controls server))
				     (setf (nth (controls-id (sv-client-controls i)) players) nil)
				     t))
				 (sv-server-clients server))))
	       ((client-p server)
		(loop
                   for i = (cl-read-packet server) while i
                   do (finish-output)
                     (ecase (first i)
                        (:choose-stage
                         (setf players (second i)))
                        (:game-start
                         (throw 'done t))))
                ;; read for keyboard events and send update if there are changes
                (let ((bk (copy-structure (elt *controls* 0))))
                  (process-events (vector (aref *controls* 0)))
                  (unless (equalp bk (elt *controls* 0))
                    (cl-send-controls server (elt *controls* 0)))))
	       (t
		;; let players tap in
		(loop for i from 0 below 4 do
		     (setf (aref bk-controls i) (copy-structure (aref *controls* i))))
		(process-events *controls*)
		(loop for i in players
		   for j from 0 do
		   (when (and (null i)
			      (not (equalp (aref bk-controls j) (aref *controls* j))))
		     (setf (nth j players) (make-ship-selection :ship 0 :wait t))))))
	     (loop
		for i in players
		for n from 0
		for color in colors
		for x in xs
		for y in ys when i do
                (unless (client-p server)
                  (if (ship-selection-wait i)
                      (setf (ship-selection-wait i) (or (controls-right (aref *controls* n))
                                                        (controls-left (aref *controls* n))
                                                        (controls-shoot (aref *controls* n))
                                                        (controls-special (aref *controls* n))))
                      (progn
                        ;;(format t "here1 ~a ~a ~a~%" angle n *controls*)
                        (setf (ship-selection-wait i)
                              (or (controls-right (aref *controls* n))
                                  (controls-left (aref *controls* n))
                                  (controls-shoot (aref *controls* n))
                                  (controls-special (aref *controls* n))))
                        (cond
                          ;; select ship
                          ((null (ship-selection-special i))
                           (cond ((controls-right (aref *controls* n))
                                  (forward ship-selection-ship (ships i) i))
                                 ((controls-left (aref *controls* n))
                                  (backward ship-selection-ship (ships i) i))
                                 ((controls-shoot (aref *controls* n))
                                  (setf (ship-selection-special i) 0))))
                          ;; select special
                          ((null (ship-selection-special-pt i))
                           (cond ((controls-right (aref *controls* n))
                                  (forward ship-selection-special (specials i) i))
                                 ((controls-left (aref *controls* n))
                                  (backward ship-selection-special (specials i) i))
                                 ((controls-shoot (aref *controls* n))
                                  (setf (ship-selection-special-pt i) 0))))
                          ;; special pts
                          ((null (ship-selection-confirm i))
                           (cond ((controls-right (aref *controls* n))
                                  (forward ship-selection-special-pt (special-pts i) i))
                                 ((controls-left (aref *controls* n))
                                  (backward ship-selection-special-pt (special-pts i) i))
                                 ((controls-shoot (aref *controls* n))
                                  (setf (ship-selection-confirm i) 0))))
                          ;; wait for them to hit something
                          (t
                           (unless (eq (ship-selection-confirm i) :done)
                             (cond ((controls-right (aref *controls* n))
                                    (reset-selection i))
                                   ((controls-left (aref *controls* n))
                                    (setf (ship-selection-confirm i) :done)))))))))
		;; draw the state
                (unless (dedicated-server-p server)
                  (sdl:draw-box-* x y width height :color (sdl:color :r 0 :g 0 :b 0) :alpha 128)
                  (sdl:draw-rectangle-* x y width height :color (color 255 255 255))
                  (cond 
                    ((null (ship-selection-special i))
                     (draw-ship-at (+ x (/ width 2)) (+ y (/ height 2)) angle scale (nth (ship-selection-ship i) (ships i)) color))
                    ((null (ship-selection-special-pt i))
                     (draw-special (nth (ship-selection-special i) (specials i)) (+ x (/ width 2)) (+ y (/ height 2))))
                    ((null (ship-selection-confirm i))
                     (draw-ship-at (+ x (/ width 2))  (+ y (/ height 2)) angle scale (nth (ship-selection-ship i) (ships i)) color)
                     (let* ((pt (nth (ship-selection-special-pt i) (special-pts i)))
                            (p (rotate-point (* scale (gun-x pt)) (* scale (gun-y pt)) angle)))
                         (sdl-gfx:draw-filled-circle-* (truncate (+ x (/ width 2) (sdl:x p))) (truncate (+ y (/ height 2) (sdl:y p))) 3 :color (color 255 255 255))
                         (sdl-gfx:draw-aa-circle-* (truncate (+ x (/ width 2) (sdl:x p))) (truncate (+ y (/ height 2) (sdl:y p))) 6 :color (color 255 255 255))))
                    (t 
                     (if (eq (ship-selection-confirm i) :done)
                         (draw-text (+ x (/ width 2)) (+ y (/ height 2)) (format nil "Player ~a is ready to ~a" (1+ n) (nth n *ready*)))
                         (draw-text (+ x (/ width 2)) (+ y (/ height 2)) "Yes  Confirm!  No"))))))
	     (unless (dedicated-server-p server)
	       (loop while (< (- (get-internal-real-time) start) (/ internal-time-units-per-second *frame-rate*)))
	       (setf start (get-internal-real-time))
               (sdl:update-display))
	     ;; in non network mode or when we're the server, we start
	     ;; when all players have finished their selections
	     (when (or (null server)
		       (and (server-p server)
			    (not (sv-server-dedicated-p server)))
		       ;; don't start the game if there are no clients
		       (and (server-p server)
			    (sv-server-dedicated-p server)
			    (plusp (length (sv-server-clients server)))))
	       (when (loop for i in players always (or (null i)
						       (eq (ship-selection-confirm i) :done)))
		 (throw 'done t)))))
	(when (server-p server)
	  (sv-send-game-start server))
	(loop for i in players
	   for color in colors
	   for j from 0
	   when i
	   collect (build-player :controls (aref *controls* j)
				 :ship (nth (ship-selection-ship i) (ships i))
				 :special (nth (ship-selection-special i) (specials i))
				 :special-pt (nth (ship-selection-special-pt i) (special-pts i))
				 :color color))))))

(defun title-screen ()
  (let ((title-font (open-font "title" 100))
        (big-font (open-font "font" 56))
        (fire (make-fire-bubbles (- (screen-width) 50) 300 20))
        (host "")
        (port 10005))
    (labels ((center (txt y &key selected (color (gray 255)) (font big-font))
               (when (plusp (length txt))
                 (sdl:draw-string-blended-* txt
                                            (truncate (- (screen-width) (sdl:get-font-size txt :size :w :font font)) 2)
                                            y
                                            :font font
                                            :color (if selected
                                                       (color 255 100 70)
                                                       color))))
             (stringize (symbol)
               (substitute #\Space #\- (string-capitalize symbol)))
             (message (line1 line2)
               (sdl:clear-display (sdl:color))
               (center line1 300)
               (center line2 350)
               (sdl:update-display)
               (wait-for-key))
             (input (title &optional (text "") (valid-key-function (constantly t)))
               (let* ((h (sdl:get-font-height :font big-font))
                      (y (truncate (- (screen-height) (sdl:get-font-height :font big-font)) 2)))
                 (loop
                    (sdl:clear-display (sdl:color))
                    (sdl:draw-box-* 0 (- y 5) (screen-width) (+ h 10) :color (color 0 30 30))
                    (sdl:draw-rectangle-* -1 (- y 5) (+ (screen-width) 2) (+ h 10) :color (color 0 60 60))
                    (center title (- (truncate (- (screen-height) h) 2)
                                     (sdl:get-font-height :font *font*) 5)
                            :font *font*
                            :color (color 0 60 60))
                    (center text (truncate (- (screen-height) h) 2))
                    (sdl:update-display)
                    (let ((ch (key-to-character (wait-for-key) nil)))
                      (when ch
                        (case ch
                          (#\Return (return text))
                          (#\Backspace (when (plusp (length text)) (setf text (subseq text 0 (1- (length text))))))
                          (t (when (funcall valid-key-function ch)
                               (setf text (concatenate 'string text (string ch)))))))))))
             (menu (options &key draw-fn (yofs 0) (key 'identity) (selected (first options)))
               (loop
                  (sdl:clear-display (gray 0))
                  (when draw-fn (funcall draw-fn))
                  (loop
                     with height = (sdl:get-font-height :font big-font)
                     with pad = 20
                     for i in options
                     for y = (+ yofs (truncate (- (screen-height) (- (* (+ height pad) (length options)) pad)) 2)) then (+ y height pad)
                     do (center (funcall key i) y :selected (eq selected i)))
                  (sdl:update-display)
                  (case (wait-for-key)
                    (:sdl-key-down (setf selected (or (second (member selected options)) (first options))))
                    (:sdl-key-up (setf selected (or (second (member selected (reverse options))) (car (last options)))))
                    (:sdl-key-return (return-from menu selected))
                    (:sdl-key-escape (return-from menu nil)))))
             (make-selection ()
               (menu '(:play-game
                       :networking
                       :options
                       :quit)
                     :draw-fn (lambda ()
                                (sdl:draw-surface-at-* fire 25 25)
                                (center "BRATWURST" 100 :font title-font :color (color 200 50 0)))
                     :yofs 200
                     :key #'stringize))
             (do-keys (keys)
               (let ((selected 'left))
                 (loop
                    (let ((slot (menu '(left right forward shoot special)
                                      :selected selected
                                      :key (lambda (control)
                                             (format nil "~:(~a~): ~a"
                                                     control
                                                     (if (slot-value keys control)
                                                         (subseq (stringize (slot-value keys control))
                                                                 (length "sdl-key-"))
                                                         "None"))))))
                      (if slot
                          (let* ((h (sdl:get-font-height :font big-font))
                                 (y (truncate (- (screen-height) (sdl:get-font-height :font big-font)) 2)))
                            (sdl:draw-box-* 0 0 (screen-width) (screen-height) :color (color 0 0 0) :alpha 200)
                            (sdl:draw-box-* 0 (- y 5) (screen-width) (+ h 10) :color (color 30 0 30))
                            (sdl:draw-rectangle-* -1 (- y 5) (+ (screen-width) 2) (+ h 10) :color (color 60 0 60))
                            (center "Press A Key" (truncate (- (screen-height) h) 2))
                            (sdl:update-display)
                            (setf (slot-value keys slot) (wait-for-key)
                                  selected slot))
                          (return nil))))))
             (do-options ()
               (let ((p (menu '(*player-1-keys*
                                *player-2-keys*
                                *player-3-keys*
                                *player-4-keys*)
                              :key (lambda (s) (string-trim '(#\*) (stringize s))))))
                 (when p
                   (do-keys (symbol-value p)))))
             (do-network ()
               (case (menu '(:start-server :connect-to-server) :key #'stringize)
                 (:start-server
                  (setf port (parse-integer (input "Port" (prin1-to-string port) 'digit-char-p)))
                  (when (eq (do-server (make-default-map) port) :error)
                    (message "Failed To Start Server" "")))
                 (:connect-to-server
                  (setf host (input "Host" host)
                        port (parse-integer (input "Port" (prin1-to-string port) 'digit-char-p)))
                  (when (eq (do-client host port) :error)
                    (message "Failed To Connect To:" host))))))
      (loop
         (catch 'main-menu
           (ecase (make-selection)
             (:play-game (do-normal-game (choose-stage) (make-default-map)))
             (:networking (do-network))
             (:options (do-options))
             ((nil :quit) (throw 'quit nil))))))))


(defun init-controls ()
  (setf *network-controls* (make-array 4)
	*controls* (make-array 4)
	(aref *controls* 0) (make-controls :id 0)
	(aref *controls* 1) (make-controls :id 1)
	(aref *controls* 2) (make-controls :id 2)
	(aref *controls* 3) (make-controls :id 3)
	(aref *network-controls* 0) (make-controls :id 0)
	(aref *network-controls* 1) (make-controls :id 1)
	(aref *network-controls* 2) (make-controls :id 2)
	(aref *network-controls* 3) (make-controls :id 3)))

(defun open-font (name size)
  (sdl-ttf:open-font (make-instance 'sdl::font-definition
                                    :filename (namestring (probe-file (make-pathname :defaults *resource-dir* :name name :type "ttf")))
                                    :size size)))

(defmacro with-graphics ((h w) &body body)
  `(sdl:with-init (sdl:sdl-init-audio)
     (sdl-mixer:open-audio)
     (sdl:window ,h ,w :title-caption "Bratwurst")
     (setf *font* (open-font "font" 16)
           *lives-font* (open-font "font" 24))
     (unwind-protect
	  (sdl:with-surface (disp sdl:*default-display*)
	    ,@body)
       (sdl-mixer:halt-music)
       (sdl-mixer:close-audio t))))

(defun screen-width ()
  (sdl:width sdl:*default-display*))

(defun screen-height ()
  (sdl:height sdl:*default-display*))

(defun game-area-width ()
  (screen-width))

(defun game-area-height ()
  (- (screen-height) (sdl:height *status-buffer*)))

(defun alive-players (players)
  (loop for i in players
       when (> (player-lives i) 0)
       collect i))

(defun dump-state (state)
  `(:state ,(mapcar 'dump-bullet (state-bullets state))
           ,(mapcar (lambda (r) (dump-missile r (state-players state))) (state-missiles state))
           ,(mapcar (lambda (r) (dump-rocket r (state-players state))) (state-rockets state))
           ,(mapcar 'dump-player (state-players state))
           ,*controls*))

(defun fill-in-color (obj)
  (setf (slot-value obj 'color) (restore-color (slot-value obj 'color))))

(defun fill-in-player (obj slot players)
  (setf (slot-value obj slot) (when (slot-value obj slot)
                                (elt players (slot-value obj slot)))))
  
(defun restore-state (state dump)
  (setf (state-bullets state) (first dump)
        (state-missiles state) (second dump)
        (state-rockets state) (third dump))
  (mapc 'fill-in-color (state-rockets state))
  (mapc 'fill-in-color (state-missiles state))
  (mapc 'fill-in-color (state-bullets state))
  (mapc (lambda (o) (fill-in-player o 'owner (state-players state))) (state-rockets state))
  (mapc (lambda (o) (fill-in-player o 'target (state-players state))) (state-missiles state))
  (mapc 'restore-player (state-players state) (fourth dump))
  (map nil 'import-controls *controls* (fifth dump)))

(defun step-game-state (state)
  "Return non-NIL when the game is over."
  (incf (state-frame state))
  (let ((livers (alive-players (state-players state))))
    (dolist (i livers)
      (setf (player-last-x i) (player-x i)
            (player-last-y i) (player-y i)
            (player-last-angle i) (player-angle i)
            (player-solidified i) nil)
      (update-player i (state-map state) (state-players state)))
    (update-for-player-player-collision livers)
    (update-bullets livers (state-map state))
    (update-rockets livers (state-map state))
    (update-missiles livers (state-map state))
    ;; keep playing?
    (<= (length (alive-players (state-players state))) 1)))

(defun draw-state (state)
  (let ((livers (alive-players (state-players state))))
    ;; calculate what part of the world to display
    (multiple-value-bind (maxx maxy minx miny)
        (loop for i in livers 
           maximize (player-x i) into maxx
           maximize (player-y i) into maxy
           minimize (player-x i) into minx
           minimize (player-y i) into miny
           finally (return (values maxx maxy minx miny)))
      (let* ((rx (max (- maxx minx) *map-max-zoom*))
             (ry (max (- maxy miny) *map-max-zoom*))
             (width (* (game-area-width) 0.75))
             (height (* (game-area-height) 0.75))
             (scale (if (> rx (* (/ width height) ry))
                        (/ width rx)
                        (/ height ry)))
             (x (- minx (/ (- (/ (game-area-width) scale) (- maxx minx)) 2))) ;;(+ minx (truncate (- maxx minx) 2))
             (y (- miny (/ (- (/ (game-area-height) scale) (- maxy miny)) 2)))) ;; (+ miny (truncate (- maxy miny)) 2))))
        ;; draw 
        (update-status-bar (state-players state))
        (draw-map (state-map state) x y scale)
        (draw-bullets x y scale)
        (draw-rockets x y scale)
        (draw-missiles x y scale)
        (dolist (i livers)
          (draw-ship (- (player-x i) x) (- (player-y i) y) (player-angle i) scale (player-ship i) (player-color i)))))))

(defun blit-buffers (time)
  (loop while (< (- (get-internal-real-time) time) (/ internal-time-units-per-second *frame-rate*)))
  (prog1
      (get-internal-real-time)
    (blit-status-bar)
    (sdl:update-display)
    (sdl:clear-display (sdl:color))))

(defun init-start-locations (state)
  (loop for i in (state-players state)
     for j in (map-starts (state-map state)) do
     (setf (player-start-x i) (first j)
           (player-x i) (first j)
           (player-start-y i) (second j)
           (player-y i) (second j))))

(defun do-normal-game (players map)
  (let* ((time (get-internal-real-time))
	 ;; state is thrown around all over the place so use a dynamic
	 ;; binding.
	 (*state* (make-state :bullets nil
			      :missiles nil
			      :rockets nil
			      :status-bar-needs-updating t
			      :players players
			      :map map
                              :frame 0))
         (*status-buffer* (sdl:create-surface (sdl:width sdl:*default-surface*) 50)))
    (init-start-locations *state*)
    (loop
       do (draw-state *state*)
          (setf time (blit-buffers time))
          (process-events *controls*)
       until (step-game-state *state*))
    ;; tell'em the deal
    (let ((player (first (alive-players (state-players *state*)))))
      (if player
          (draw-text 320 240 "You Are The Winner!" (player-color player))
          (draw-text 320 240 "Draw Game!")))
    (sdl:update-display)
    (loop until (eq (wait-for-key) :sdl-key-escape))))

(defun do-client-game (players map server)
  (let* ((time (get-internal-real-time))
	 ;; state is thrown around all over the place so use a dynamic
	 ;; binding.
	 (*state* (make-state :bullets nil
			      :missiles nil
			      :rockets nil
			      :status-bar-needs-updating t
			      :players players
			      :map map
                              :frame 0))
         (*status-buffer* (sdl:create-surface (sdl:width sdl:*default-surface*) 50))
         (controls (make-controls))
         (winner))
    (init-start-locations *state*)
    (setf winner
          (catch 'done
            (loop do
                 (draw-state *state*)
                 (setf time (blit-buffers time))
                 (let ((bk (copy-structure controls)))
                   (process-events (vector controls))
                   (unless (equalp bk controls)
                     (cl-send-controls server controls)))
                 (with-cl-packets (server)
                   (:state
                    (restore-state *state* (cdr %packet%)))
                   (:winner
                    (throw 'done (elt (state-players *state*) (second %packet%)))))
                 (step-game-state *state*))))
    ;; tell'em the deal
    (if winner
        (draw-text 320 240 "You Are The Winner!" (player-color winner))
        (draw-text 320 240 "Draw Game!"))
    (sdl:update-display)
    (loop until (eq (wait-for-key) :sdl-key-escape))))

(defun do-server-game (players map server)
  (let* ((time (get-internal-real-time))
	 ;; state is thrown around all over the place so use a dynamic
	 ;; binding.
	 (*state* (make-state :bullets nil
			      :missiles nil
			      :rockets nil
			      :status-bar-needs-updating t
			      :players players
			      :map map
                              :frame 0))
         (*status-buffer* (sdl:create-surface (sdl:width sdl:*default-surface*) 50))
         (bk-controls (make-empty-controls-array 4)))
    (init-start-locations *state*)
    (loop do
         (draw-state *state*)
         (setf time (blit-buffers time))

         (loop for i below 4 do
              (setf (aref bk-controls i) (copy-structure (aref *controls* i))))
         (process-events (vector (aref *controls* 0)))
         (with-sv-packets (server)
           (:controls
            (import-controls (sv-client-controls %client%) (second %packet%))))
         (when (notevery 'equalp bk-controls *controls*)
           (sv-send-packet server (dump-state *state*)))

         until (step-game-state *state*))
    (let ((winner (first (alive-players (state-players *state*)))))
      (sv-send-packet server `(:winner ,(position winner (state-players *state*))))
      ;; tell'em the deal
      (if winner
          (draw-text 320 240 "You Are The Winner!" (player-color winner))
          (draw-text 320 240 "Draw Game!"))
      (sdl:update-display)
      (loop until (eq (wait-for-key) :sdl-key-escape)))))

(defun do-dedicated-server-game (players map server)
  (let* ((time (get-internal-real-time))
	 ;; state is thrown around all over the place so use a dynamic
	 ;; binding.
	 (*state* (make-state :bullets nil
			      :missiles nil
			      :rockets nil
			      :status-bar-needs-updating t
			      :players players
			      :map map
                              :frame 0))
         (*status-buffer* (sdl:create-surface (sdl:width sdl:*default-surface*) 50))
         (bk-controls (make-empty-controls-array 4)))
    (init-start-locations *state*)
    (loop do
         (loop while (< (- (get-internal-real-time) time) (/ internal-time-units-per-second *frame-rate*)))
         (setf time (get-internal-real-time))
         (loop for i below 4 do
              (setf (aref bk-controls i) (copy-structure (aref *controls* i))))
         (with-sv-packets (server)
           (:controls
            (import-controls (sv-client-controls %client%) (second %packet%))))
         (when (notevery 'equalp bk-controls *controls*)
           (sv-send-packet server (dump-state *state*)))
         until (step-game-state *state*))
    (let ((winner (first (alive-players (state-players *state*)))))
      (sv-send-packet server `(:winner ,(position winner (state-players *state*)))))))

(defun do-server (map &optional (port 10005))
  (let ((server (start-server (list (aref *controls* 1)
                                    (aref *controls* 2)
                                    (aref *controls* 3))
                              nil port)))
    ;; the player on the server always uses controls 0 but we
    ;; dont want any of the other keys to change things, so
    ;; when we process events and fill network-controls this
    ;; allows controls 0 to be updated. it's a hack.
    (if server
        (progn
          (setf (aref *network-controls* 0) (aref *controls* 0))
          (unwind-protect
               (do-server-game (choose-stage server) map server)
            (close-server server)))
        :error)))

(defun do-client (host &optional (port 10005))
  (let ((server (cl-start-client host (aref *network-controls* 0) port)))
    (if server
        (unwind-protect
            (do-client-game (choose-stage server) (make-default-map) server)
          (close-client server))
        :error)))
  
(defun bratwurst ()
  "play a game of bratwurst with N players"
  (catch 'quit
    (init-controls)
    (with-graphics (1024 768)
      (title-screen))))

(defun update-controls (controls sym press)
  (labels ((update (control player)
             (cond
               ((eq sym (controls-left player))
                (setf (controls-left control) press))
               ((eq sym (controls-right player))
                (setf (controls-right control) press))
               ((eq sym (controls-forward player))
                (setf (controls-forward control) press))
               ((eq sym (controls-special player))
                (setf (controls-special control) press))
               ((eq sym (controls-shoot player))
                (setf (controls-shoot control) press)))))
    (loop
       for c across controls
       for p in (list *player-1-keys*
                      *player-2-keys*
                      *player-3-keys*
                      *player-4-keys*)
       do (update c p))
    (when (eq sym :sdl-key-escape)
      (throw 'main-menu t))))

(defun wait-for-key ()
  (labels ((match-event (event type)
             (eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type type)
                  (cffi:foreign-slot-value event 'sdl-cffi::sdl-event 'sdl-cffi::type)))
           (keysym (event)
             (cffi:foreign-slot-value (cffi:foreign-slot-pointer event
                                                                 'sdl-cffi::sdl-keyboard-event
                                                                 'sdl-cffi::keysym)
                                      'sdl-cffi::sdl-key-sym 'sdl-cffi::sym)))
    (let ((event (sdl:new-event)))
      (loop while (plusp (sdl-cffi::sdl-wait-event event))
         do (cond ((match-event event :sdl-key-down-event)
                   (return-from wait-for-key (keysym event))))))))

(defun process-events (controls)
  (labels ((match-event (event type)
             (eql (cffi:foreign-enum-value 'sdl-cffi::Sdl-Event-Type type)
                  (cffi:foreign-slot-value event 'sdl-cffi::sdl-event 'sdl-cffi::type)))
           (keysym (event)
             (cffi:foreign-slot-value (cffi:foreign-slot-pointer event
                                                                 'sdl-cffi::sdl-keyboard-event
                                                                 'sdl-cffi::keysym)
                                      'sdl-cffi::sdl-key-sym 'sdl-cffi::sym)))
    (let ((event (sdl:new-event)))
      (loop while (plusp (sdl-cffi::sdl-poll-event event))
         do (cond ((match-event event :sdl-key-down-event)
                   (update-controls controls (keysym event) t))
                  ((match-event event :sdl-key-up-event)
                   (update-controls controls (keysym event) nil)))))))
