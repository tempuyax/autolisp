;;--------------------=={ Get Syntax }==----------------------;;
;;                                                            ;;
;;  Returns a list of syntax for all defined commands in a    ;;
;;  supplied LISP file.                                       ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  file - filename of LISP file to read                      ;;
;;------------------------------------------------------------;;
;;  Returns:  List of defined commands in supplied LISP file  ;;
;;------------------------------------------------------------;;

(defun LM:GetSyntax ( file / _GetSyntax line syntax )

  (defun _GetSyntax ( p s / x )
    (if (setq x (vl-string-search p s))
      (cons
        (substr (setq s (substr s (+ x 1 (strlen p)))) 1
          (setq x
            (car
              (vl-sort
                (vl-remove 'nil
                  (mapcar
                    (function
                      (lambda ( d ) (vl-string-position d s))
                    )
                   '(32 9 40 41)
                  )
                )
                '<
              )
            )
          )
        )
        (if x (_GetSyntax p (substr s (1+ x))))
      )
    )
  ) 

  (if (setq file (open file "r"))
    (apply 'append
      (progn
        (while (setq line (read-line file))
          (setq syntax (cons (_GetSyntax "(DEFUN C:" (strcase line)) syntax))
        )
        (setq file (close file)) (reverse syntax)
      )
    )
  )
)