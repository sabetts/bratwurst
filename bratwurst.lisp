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

(defpackage #:bratwurst
  (:use cl)
  (:export #:bratwurst #:bratwurst-server #:bratwurst-dedicated-server #:bratwurst-client))

(in-package #:bratwurst)

;; fucking clisp signals floating-point-underflow
#+clisp (handler-case (setf sys::*inhibit-floating-point-underflow* t)
	  (error (c)
	    (declare (ignore c))
	    (when (find-restart 'continue)
	      (continue))))

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
  players map)

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
  (declare (ignore a))
  (sdl:color :r r :g g :b b))

(defun gray (intensity)
  (color intensity intensity intensity))

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
    (sdl:draw-box-* (+ x 50) (+ y 3) 90 5 :surface drawable :color (gray 150))
    (sdl:draw-box-* (+ x 50) (+ y 3) (truncate (* 90 (/ (player-health player) (ship-max-health (player-ship player))))) 5 :surface drawable :color (gray 200))
    (sdl:draw-box-* (+ x 50) (+ y 10) 90 5 :surface drawable :color (gray 150))
    (sdl:draw-box-* (+ x 50) (+ y 10) (truncate (* 90 (/ (player-ammo player) (ship-max-ammo (player-ship player))))) 5 :surface drawable :color (gray 200))
    (sdl:draw-box-* (+ x 50) (+ y 17) 90 5  :surface drawable :color (gray 150))
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
  (sdl:draw-string-blended-* txt (- x (truncate (sdl:get-font-size txt :size :w :font *font*) 2)) y :color color :font *font*))

(defun draw-special (special x y)
  (let ((txt (ecase special
	       (:none "No special")
	       (:missile "Missile")
	       (:rocket "Rocket")
	       (:tractor "Tractor Beam")
	       (:turret "Turret")
	       (:booster "Booster"))))
    (draw-text x y txt)))

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
      (let ((players (list (make-ship-selection :ship 0) nil nil nil))
	    (angle 0)
	    (colors (list *player1-color* *player2-color* *player3-color* *player4-color*))
	    (xs '(25 350 25 350))
	    (ys '(25 25 200 200))
	    (width 250)
	    (height 150)
	    (start (get-internal-real-time))
	    (bk-controls #(nil nil nil nil)))
	(catch 'done
	  (loop
             (sdl:clear-display (sdl:color))
	     (setf angle (mod (+ angle 2) 360))
	     ;; depending on the server type we handle event
	     ;; processing differently.
	     (cond 
	       ((server-p server)
		;; anyone joined the game?
		(let ((new (sv-check-for-new-clients server players)))
		  (when new
		    (setf (nth (controls-id new) players) (make-ship-selection :ship 0))))
		;; backup the controls so we can see if any changed.
		(loop for i from 0 below 4 do
		     (setf (aref bk-controls i) (copy-structure (aref *controls* i))))
		;; check for key events
		(unless (sv-server-dedicated-p server)
		  (network-process-events))
		(sv-read-packets server)
		;; if someone disconnected then return their controls
		;; and zap their ship selection
		(setf (sv-server-clients server)
		      (remove-if (lambda (i)
				   (when (sv-client-disconnected-p i)
				     (push (sv-client-controls i) (sv-server-free-controls server))
				     (setf (nth (controls-id (sv-client-controls i)) players) nil)
				     t))
				 (sv-server-clients server)))
		;; send a controls packet if anything changed
		(unless (loop for j from 0 below 4
			   always (equalp (aref bk-controls j) (aref *controls* j)))
		  (sv-send-controls server 0))
		)
	       ((client-p server)
		;; read only 1 packet at a time. this avoids the
		;; problem of reading 2 controls updates.
		(cl-read-packets server)
		;; check for new players
		(loop for j in (cl-server-players server) do
		     (unless (nth (controls-id j) players)
		       (setf (nth (controls-id j) players) (make-ship-selection :ship 0))))
		;; check for selections (happens when we first join)
		(when (cl-server-others server)
		  (setf players (cl-server-others server)
			(cl-server-others server) nil))
                ;; read for keyboard events and send update if there are changes
                (let ((bk (copy-structure (cl-server-controls server))))
                  (network-process-events)
                  (unless (equalp bk (cl-server-controls server))
                    (cl-send-controls server))))
	       (t
		;; let players tap in
		(loop for i from 0 below 4 do
		     (setf (aref bk-controls i) (copy-structure (aref *controls* i))))
		(process-events)
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
				  (setf (ship-selection-confirm i) :done))))))))
		;; draw the state
                (unless (dedicated-server-p server)
                  (cond 
                    ((null (ship-selection-special i))
                     (draw-ship (+ x (/ width 2)) (+ y (/ height 2)) angle 1 (nth (ship-selection-ship i) (ships i)) color))
                    ((null (ship-selection-special-pt i))
                     (draw-special (nth (ship-selection-special i) (specials i)) (+ x (/ width 2)) (+ y (/ height 2))))
                    ((null (ship-selection-confirm i))
                     (draw-ship (+ x (/ width 2))  (+ y (/ height 2)) angle 1 (nth (ship-selection-ship i) (ships i)) color)
                     (let* ((pt (nth (ship-selection-special-pt i) (special-pts i)))
                            (p (rotate-point (gun-x pt) (gun-y pt) angle)))
                         (sdl-gfx:draw-filled-circle-* (truncate (+ x (/ width 2) (sdl:x p))) (truncate (+ y (/ height 2) (sdl:y p))) 3 :color (color 255 255 255))))
                    (t 
                     (if (eq (ship-selection-confirm i) :done)
                         (draw-text (+ x (/ width 2)) (+ y (/ height 2)) (format nil "Player ~a is ready to ~a" (1+ n) (nth n *ready*)))
                         (draw-text (+ x (/ width 2)) (+ y (/ height 2)) "Yes  Confirm!  No"))))
                  (sdl:draw-rectangle-* x y width height :color (color 255 255 255))))
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

(defmacro with-graphics ((h w) &body body)
  `(sdl:with-init (sdl:sdl-init-audio)
     (sdl-mixer:open-audio)
     (sdl:window ,h ,w :title-caption "Bratwurst")
     (setf *font* (sdl-ttf:open-font (make-instance 'sdl::font-definition
                                                    :filename (namestring (probe-file "font.ttf"))
                                                    :size 16))
           *lives-font* (sdl-ttf:open-font (make-instance 'sdl::font-definition
                                                          :filename (namestring (probe-file "font.ttf"))
                                                          :size 24)))
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

(defun do-game (players map &optional server)
  (let* ((x 0)
	 (y 0)
	 (scale 1)
	 (frames 0)
	 (start (get-internal-real-time))
	 (bk-controls (make-empty-controls-array 4))
	 ;; state is thrown around all over the place so use a dynamic
	 ;; binding.
	 (*state* (make-state :bullets nil
			      :missiles nil
			      :rockets nil
			      :status-bar-needs-updating t
			      :players players
			      :map map))
	 ;; this stores the state of the game after the last server
	 ;; message. If we get a server message for a frame earlier
	 ;; than the frame we're on then we jump back to this last
	 ;; state, replay the game up to the server message and
	 ;; then keep going. If we get a server message after the
	 ;; frame we're on we simply zip the gamestate forward to
	 ;; catch up. 
	 ;;
	 ;; FIXME: This will cause chunks as the ping time
	 ;; fluxuates. A better way might be to keep track of the
	 ;; ping time or average ping time and try to stay that
	 ;; many seconds behind the server.
	 (last-server-state (backup-state *state*))
	 (last-server-state-controls (make-empty-controls-array 4))
         (*status-buffer* (sdl:create-surface (sdl:width sdl:*default-surface*) 50)))
    (copy-controls-to *controls* last-server-state-controls)
    ;; reset the last server message since we haven't gotten any yet.
    (when (client-p server)
      (setf (cl-server-last-frame server) -1))
    (loop for i in (state-players *state*)
       for j in (map-starts map) do
       (setf (player-start-x i) (first j)
	     (player-x i) (first j)
	     (player-start-y i) (second j)
	     (player-y i) (second j)))
    (catch 'done
      (labels ((step-game-state ()
		 (incf frames)
		 (let ((livers (alive-players (state-players *state*))))
		   (dolist (i livers)
		     (setf (player-last-x i) (player-x i)
			   (player-last-y i) (player-y i)
			   (player-last-angle i) (player-angle i)
			   (player-solidified i) nil)
		     (update-player i map (state-players *state*)))
		   (update-for-player-player-collision livers)
		   (update-bullets livers map)
		   (update-rockets livers map)
		   (update-missiles livers map)))
	       ;; draw everything to the double buffer
	       (draw-game ()
		 (let ((livers (alive-players (state-players *state*))))
		   ;; calculate what part of the world to display
		   (multiple-value-bind (maxx maxy minx miny)
		       (loop for i in livers 
			  maximize (player-x i) into maxx
			  maximize (player-y i) into maxy
			  minimize (player-x i) into minx
			  minimize (player-y i) into miny
			  finally (return (values maxx maxy minx miny)))
		     (let ((rx (max (- maxx minx) *map-max-zoom*))
			   (ry (max (- maxy miny) *map-max-zoom*))
                           (width (* (game-area-width) 0.75))
                           (height (* (game-area-height) 0.75)))
		       (setf scale
			     (if (> rx (* (/ width height) ry))
				 (/ width rx)
				 (/ height ry))
			     x (- minx (/ (- (/ (game-area-width) scale) (- maxx minx)) 2)) ;;(+ minx (truncate (- maxx minx) 2))
			     y (- miny (/ (- (/ (game-area-height) scale) (- maxy miny)) 2))))) ;; (+ miny (truncate (- maxy miny)) 2))))
		   ;; draw 
		   (update-status-bar (state-players *state*))
		   (draw-map map x y scale)
		   (draw-bullets x y scale)
		   (draw-rockets x y scale)
		   (draw-missiles x y scale)
		   (dolist (i livers)
		     (draw-ship (- (player-x i) x) (- (player-y i) y) (player-angle i) scale (player-ship i) (player-color i)))))
	       ;; blitting the buffers and frame syncing
	       (blit-buffers ()
		 (loop while (< (- (get-internal-real-time) start) (/ internal-time-units-per-second *frame-rate*)))
		 (setf start (get-internal-real-time))
		 (when (<= (length (alive-players (state-players *state*))) 1)
		   (throw 'done t))
		 (blit-status-bar)
                 (sdl:update-display)
                 (sdl:clear-display (sdl:color)))
	       ;; when a packet comes in we gotta shuffle things around
	       (handle-packet (frame)
		 (cond ((< (cl-server-last-frame server) frames)
			;;(format t "here 1 ~a ~a ~a~%" frame (cl-server-last-frame server) frames)
			;; restore to the last server message and step up to the new server packet
			(setf *state* last-server-state
			      frames (cl-server-last-frame server))
			(copy-controls-to *controls* bk-controls)
			(copy-controls-to last-server-state-controls *controls*)
			(dotimes (i (- (cl-server-last-frame server) frame 1))
			  (step-game-state))
			;; now restore the controls we got from the
			;; packet (it could have been a heart beart
			;; actually) and go to step the state.
			(copy-controls-to bk-controls *controls*)
			(copy-controls-to bk-controls last-server-state-controls)
			(step-game-state)
			(setf last-server-state (backup-state *state*)))
		       ((> (cl-server-last-frame server) frames)
			;;(format t "here 2 ~a ~a ~a~%" frame (cl-server-last-frame server) frames)
			;; we're behind so catch up.
			(copy-controls-to *controls* last-server-state-controls)
			(copy-controls-to bk-controls *controls*)
			(dotimes (i (- (cl-server-last-frame server) frames 1))
			  (step-game-state))
			;; okay we're all set to play the latest packet
			(copy-controls-to last-server-state-controls *controls*)
			(step-game-state)
			(setf last-server-state (backup-state *state*))
			(assert (= frames (cl-server-last-frame server))))
		       ;; frames is right on schedule with the server
		       ;; packet, so get a backup and thats it.
		       (t
			;;(format t "here 3 ~a ~a ~a~%" frame (cl-server-last-frame server) frames)
			(copy-controls-to *controls* last-server-state-controls)
			(step-game-state)
			(setf last-server-state (backup-state *state*))))))
	(loop ;; for i from 1 to 10000 do
	   ;; read input
	   (cond
	     ((server-p server)
	      ;; backup the controls so we can see if any changed.
	      (loop for i from 0 below 4 do
		   (setf (aref bk-controls i) (copy-structure (aref *controls* i))))
	      ;; check for key events
	      (unless (sv-server-dedicated-p server)
		(network-process-events))
	      (sv-read-all-packets server)
	      ;; get rid of the disconnects
	      (setf (sv-server-clients server) (delete t (sv-server-clients server) :key 'sv-client-disconnected-p))
	      ;; no point in going on if everyone disconnected
	      (when (and (sv-server-dedicated-p server)
			 (null (sv-server-clients server)))
		(throw 'done t))
	      ;; send a controls packet if anything changed, otherwise heartbeat
	      (if (loop for j from 0 below 4
		     always (equalp (aref bk-controls j) (aref *controls* j)))
		  ;; heartbeats are sent every 10 frames
		  (when (zerop (mod frames 10))
		    (sv-send-heartbeat server frames))
		  (sv-send-controls server frames))
	      (step-game-state))
	     ((client-p server)
	      (let ((frame (cl-server-last-frame server)))
		;; 		(if (zerop (mod frames 10))
		;; 		    ;; wait for the server packets to catch
		;; 		    ;; up. specifically wait for the sync
		;; 		    ;; packet. that's sent every 10 frames.
		;; 		    (loop until (<= frames (cl-server-last-frame server)) do
		;; 			 (copy-controls-to *controls* bk-controls)
		;; 			 (cl-blocking-read-packets server)
		;; 			 (handle-packet frame)
		;; 			 (setf frame (cl-server-last-frame server)))
		;; read a packet if there's one to read
		;; 		    (progn
		;; ugh. we need to backup the controls here
		(copy-controls-to *controls* bk-controls)
		(cl-read-packets server)
		;; did a packet come in?
		;;(format t "client: ~d server: ~d~%" frame (cl-server-last-frame server))
		(when (/= frame (cl-server-last-frame server))
		  (handle-packet frame)))
		
	      ;; read for keyboard events and send update if there are changes
	      (let ((bk (copy-structure (cl-server-controls server))))
		(network-process-events)
		(unless (equalp bk (cl-server-controls server))
		  (cl-send-controls server))))
	     (t
	      (process-events)
	      (step-game-state)))
	   ;; dedicated servers don't draw anything
	   (unless (and (server-p server)
			(sv-server-dedicated-p server))
	     (draw-game)
	     (blit-buffers)))))
    ;;(format t "time elapsed: ~f~%" (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    ;; tell'em the deal
    (unless (and (server-p server)
		 (sv-server-dedicated-p server))
      (let ((player (first (alive-players (state-players *state*)))))
	(if player
	    (draw-text 320 240 "You Are The Winner!" (player-color player))
	    (draw-text 320 240 "Draw Game!")))
      (loop
	 (blit-status-bar)
         (sdl:update-display)
	 (process-events)))))

(defun bratwurst-server (&optional (port 10005))
  (catch 'quit
    (init-controls)
    (with-graphics (1024 768)
      (let ((server (start-server (list (aref *controls* 1)
					(aref *controls* 2)
					(aref *controls* 3))
				  nil port)))
	;; the player on the server always uses controls 0 but we
	;; dont want any of the other keys to change things, so
	;; when we process events and fill network-controls this
	;; allows controls 0 to be updated. it's a hack.
	(setf (aref *network-controls* 0) (aref *controls* 0))
	(do-game (choose-stage server) (make-default-map) server))
      (close-server))))

(defun bratwurst-dedicated-server (&optional (port 10005))
  (catch 'quit
    ;; dedicated servers endlessly host games
    (loop
       (init-controls)
       (unwind-protect
	    (let ((server (start-server (list (aref *controls* 0)
					      (aref *controls* 1)
					      (aref *controls* 2)
					      (aref *controls* 3))
					t port)))
	      (do-game (choose-stage server) (make-default-map) server))
	 (close-server)))))

(defun bratwurst-client (host &optional (port 10005))
  (catch 'quit
    (init-controls)
    (let ((server (cl-start-client host (aref *network-controls* 0) port)))
      (with-graphics (1024 768)
	   (do-game (choose-stage server) (make-default-map) server)))))
  
(defun bratwurst ()
  "play a game of bratwurst with N players"
  (catch 'quit
    (init-controls)
    (with-graphics (1024 768)
      (do-game (choose-stage) (make-default-map)))))

(defun update-controls (sym press)
  (case sym
    ;; player 1
    (:sdl-key-h ;; h #x0061
     (setf (controls-left (aref *controls* 0)) press))
    (:sdl-key-n ;; n #x0064
     (setf (controls-right (aref *controls* 0)) press))
    (:sdl-key-c ;; c #x0077
     (setf (controls-forward (aref *controls* 0)) press))
    (:sdl-key-l ;; shift_l
     (setf (controls-special (aref *controls* 0)) press))
    (:sdl-key-space ;; semicolon
     (setf (controls-shoot (aref *controls* 0)) press))

    ;; player 2
    (:sdl-key-a ;; a
     (setf (controls-left (aref *controls* 1)) press))
    (:sdl-key-e ;; e
     (setf (controls-right (aref *controls* 1)) press))
    (:sdl-key-comma ;; ,
     (setf (controls-forward (aref *controls* 1)) press))
    (:sdl-key-o ;; o
     (setf (controls-special (aref *controls* 1)) press))
    (:sdl-key-semicolon ;; space
     (setf (controls-shoot (aref *controls* 1)) press))

    (:sdl-key-escape ;; esc
     (throw 'quit t))))

(defun handle-key-press (sym)
  (update-controls sym t))

(defun handle-key-release (sym)
  (update-controls sym nil))

(defun network-process-events ()
  "call this if we're in a network game. we need to override the controls."
  (let ((*controls* *network-controls*))
    (process-events)))

(defun process-events ()
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
                   (handle-key-press (keysym event)))
                  ((match-event event :sdl-key-up-event)
                   (handle-key-release (keysym event))))))))

;;; some naive networking. currently entirely dependant on clisp.

;; start the game for N players. players join the server and are given
;; a player id 0 - 3. controls are mapped to network streams or
;; keyboards.

(defstruct cl-server
  socket controls
  ;; a list of indexes into *controls* for the other players. this is
  ;; used when choosing the stage to detect new players
  players
  ;; a list of the existing player choose states when this client
  ;; joined
  others
  ;; the last frame from the server
  last-frame)

(defstruct sv-client
  frame-packet-read-p
  socket controls stream
  ;; this is set to T when we get they close their connection
  disconnected-p)

(defstruct sv-server
  socket
  free-controls
  clients
  dedicated-p)

(defvar *server* nil)

(defun server-p (&optional (server *server*))
  "are we the server?"
  (typep server 'sv-server))

(defun dedicated-server-p (&optional (server *server*))
  "are we the server?"
  (and (server-p server)
       (sv-server-dedicated-p server)))

(defun client-p (&optional (server *server*))
  "are we a client?"
  (typep server 'cl-server))

(defun start-server (free-controls dedicated-p port)
  (setf *server* (make-sv-server :socket (usocket:socket-listen usocket:*wildcard-host* port :backlog 4)
				 :free-controls free-controls
				 :clients nil
				 :dedicated-p dedicated-p))
  (format t "Waiting for connections on ~S:~D~%"
	  (usocket:get-local-name (sv-server-socket *server*))
	  (usocket:get-local-port (sv-server-socket *server*)))
  *server*)

(defun close-server ()
  (usocket:socket-close (sv-server-socket *server*)))
  
(defun sv-send-packet-to (client packet)
  (format (sv-client-socket client) "~s" packet))

(defun sv-send-packet (server packet)
  (loop for i in (sv-server-clients server) do
       (sv-send-packet-to i packet)))

(defun sv-send-join (server controls)
  (sv-send-packet server `(:join ,controls)))

(defun sv-send-selections (client selections)
  (sv-send-packet-to client `(:others ,@selections)))

(defun sv-send-controls (server frame)
  ;;(format t "Sent controls packet ~d~%" frame)
  (sv-send-packet server `(:controls ,frame
				     ,(controls-list (aref *controls* 0))
				     ,@(loop for i in (sv-server-clients server)
					  collect (controls-list (sv-client-controls i))))))

(defun sv-send-heartbeat (server frame)
  (sv-send-packet server `(:heartbeat ,frame)))

(defun sv-send-game-start (server)
  (sv-send-packet server `(:game-start)))

(defun sv-check-for-new-clients (server selections)
  (when (listen (sv-server-socket server))
    (let* ((socket (usocket:socket-accept (sv-server-socket server)))
	   (controls (pop (sv-server-free-controls server)))
	   (client (make-sv-client
		    :socket socket
		    :controls controls)))
      ;; tell them about it
      (format t "Client connecting from ~S:~D to ~S:~D~%"
              (usocket:get-peer-name socket) (usocket:get-peer-port socket)
              (usocket:get-local-name socket) (usocket:get-local-port socket))
      ;; tell the joining player about everyone else
      (sv-send-selections client selections)
      (push client (sv-server-clients server))
      ;; tell all the players about the join.
      (sv-send-join server controls)
      controls)))
    
(defun import-controls (c data)
  (when data
    (setf (controls-left c) (first data)
	  (controls-right c) (second data)
	  (controls-forward c) (third data)
	  (controls-special c) (fourth data)
	  (controls-shoot c) (fifth data))))

(defun sv-handle-client-packet (client packet)
  (let ((type (first packet))
	(data (rest packet)))
    (ecase type
      (:heartbeat
       ;; the client did nothing so they just need to send a heartbeat
       (setf (sv-client-frame-packet-read-p client) t))
      (:controls
       ;; the client pressed or released keys so here's the current
       ;; state of the controls
       (let ((c (sv-client-controls client)))
	 (import-controls c data)
	 (setf (sv-client-frame-packet-read-p client) t))))))

(defun sv-read-client-packets (server client)
  "packets are lists. car is the id and the rest is data."
  (declare (ignore server))
  (when (listen (sv-client-socket client))
    (handler-case
        (sv-handle-client-packet client (read (sv-client-socket client)))
      (end-of-file ()
        (let ((socket (sv-client-socket client)))
          (format t "Client from ~S:~D Disconnected!~%"
                  (usocket:get-peer-name socket) (usocket:get-peer-port socket))
          (setf (sv-client-disconnected-p client) t)
          nil)))))

(defun sv-read-packets (server)
  (loop for i in (sv-server-clients server) do
       (sv-read-client-packets server i)))

(defun sv-read-all-packets (server)
  (loop for i in (sv-server-clients server) do
       (loop while (sv-read-client-packets server i))))

(defun cl-send-heartbeat (server)
  (write-string "(:heartbeat)" (cl-server-socket server)))

(defun cl-send-controls (server)
  (let ((c (cl-server-controls server)))
    (format (cl-server-socket server) "(:controls ~a ~a ~a ~a ~a)"
	    (controls-left c)
	    (controls-right c)
	    (controls-forward c)
	    (controls-special c)
	    (controls-shoot c))))

(defun cl-handle-server-packet (server packet)
  (let ((type (first packet))
	(data (rest packet)))
    (ecase type
      ;; nothing has changed. so you just get a heartbeat
      (:heartbeat
       ;;(format t "heartbeat for frame ~d!~%" (first data))
       (setf (cl-server-last-frame server) (first data)))
      ;; someone joined the game. data contains the controls index
      (:join 
       (push (first data) (cl-server-players server)))
      ;; the state of the others when this client joined. this is set
      ;; and then bubbles up to the choose ship part.
      (:others
       (setf (cl-server-others server) data))
      ;; an update to 1 or more of the controls
      (:controls
       ;; data will always have up to 4 elements corresponding to each
       ;; player. if its nil then there were no changes to that player's controls
       ;; the second arg is the frame number
       ;;(format t "controls for frame ~d!~%" (first data))
       (setf (cl-server-last-frame server) (pop data))
       (loop
	  for i in data do
	  (import-controls (aref *controls* (car i)) (cdr i))))
      ;; this really seems like the wrong place to do this. 
      (:game-start
       (throw 'done t)))))

(defun cl-read-packets (server)
  (when (listen (cl-server-socket server))
    (cl-handle-server-packet server (read (cl-server-socket server)))
    t))

(defun cl-blocking-read-packets (server)
  (cl-handle-server-packet server (read (cl-server-socket server))))

(defun cl-start-client (host controls port)
  (format t "Attempting to connect to server ~S:~D~%" host port)
  (setf *server* (make-cl-server :socket (usocket:socket-connect port host)
				 :controls controls
				 :players nil))
  (format t "Connected!~%")
  *server*)
