(in-package :bratwurst)

(defvar *keysym-alist*
  `((:sdl-key-a #\a #\A)
    (:sdl-key-b #\b #\B)
    (:sdl-key-c #\c #\C)
    (:sdl-key-d #\d #\D)
    (:sdl-key-e #\e #\E)
    (:sdl-key-f #\f #\F)
    (:sdl-key-g #\g #\G)
    (:sdl-key-h #\h #\H)
    (:sdl-key-i #\i #\I)
    (:sdl-key-j #\j #\J)
    (:sdl-key-k #\k #\K)
    (:sdl-key-l #\l #\L)
    (:sdl-key-m #\m #\M)
    (:sdl-key-n #\n #\N)
    (:sdl-key-o #\o #\O)
    (:sdl-key-p #\p #\P)
    (:sdl-key-q #\q #\Q)
    (:sdl-key-r #\r #\R)
    (:sdl-key-s #\s #\S)
    (:sdl-key-t #\t #\T)
    (:sdl-key-u #\u #\U)
    (:sdl-key-v #\v #\V)
    (:sdl-key-w #\w #\W)
    (:sdl-key-x #\x #\X)
    (:sdl-key-y #\y #\Y)
    (:sdl-key-z #\z #\Z)
    ;; others
    (:sdl-key-tab #\Tab)
    (:sdl-key-space #\Space)
    (:sdl-key-return #\Return)
    (:sdl-key-backspace #\Backspace)
    ;; numbers
    (:sdl-key-1 #\1 #\!)
    (:sdl-key-2 #\2 #\@)
    (:sdl-key-3 #\3 #\#)
    (:sdl-key-4 #\4 #\$)
    (:sdl-key-5 #\5 #\%)
    (:sdl-key-6 #\6 #\^)
    (:sdl-key-7 #\7 #\&)
    (:sdl-key-8 #\8 #\*)
    (:sdl-key-9 #\9 #\()
    (:sdl-key-0 #\0 #\))
    ;; punctuation
    (:sdl-key-equals #\= #\+)
    (:sdl-key-minus #\- #\_)
    (:sdl-key-backslash #\\ #\|)
    (:sdl-key-quote #\' #\")
    (:sdl-key-backquote #\` #\~)
    (:sdl-key-period #\. #\>)
    (:sdl-key-comma #\, #\<)
    (:sdl-key-slash #\/ #\?)
    (:sdl-key-semicolon #\; #\:)
    (:sdl-key-leftbracket #\[ #\{)
    (:sdl-key-rightbracket #\] #\})
    ))

(defun key-to-character (sym shiftp)
  "Convert key to a character or return NIL if there is no conversion for the key."
  (let ((ch (find sym *keysym-alist* :key 'first)))
    (if shiftp
        (or (third ch) (second ch))
        (second ch))))
