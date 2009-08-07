;;; -*- lisp -*-

(defpackage #:bratwurst-system
  (:use #:cl #:asdf))

(in-package #:bratwurst-system)

(defsystem bratwurst
    :depends-on (lispbuilder-sdl lispbuilder-sdl-ttf lispbuilder-sdl-image lispbuilder-sdl-mixer lispbuilder-sdl-gfx usocket)
    :serial t
    :components ((:file "bratwurst")))
