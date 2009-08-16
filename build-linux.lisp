(require 'asdf)
(require 'bratwurst)
(handler-bind
    ((cffi:load-foreign-library-error (lambda (c)
                                       (use-value "./liblispbuilder-sdl-ttf-glue.so"))))
  (cffi:use-foreign-library lispbuilder-sdl-ttf-cffi::sdl-ttf-glue))

(defun main ()
  (setf bratwurst::*resource-dir* "./")
  (bratwurst::bratwurst)
  0)

(sb-ext:save-lisp-and-die "bratwurst" :toplevel #'main :executable t)
