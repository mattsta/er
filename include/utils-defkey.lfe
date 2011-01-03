(eval-when-compile
 ; (caps? $A) -> true
 ; (caps? $e) -> false
 (defun caps?
  ((x) (when (andalso (is_integer x) (>= x 65) (=< x 90))) 'true)
  ((_) 'false))
 ; (extract-caps (a b C d E f)) -> (C E)
 (defun extract-caps
  ([()] '())
  ([(x . xs)] (let* ((first-character (car (l x))))
               (cond
                ((caps? first-character) (cons x (extract-caps xs)))
                (else (extract-caps xs))))))
 ; (listize-parts (a b C d E f) (C E)) -> (#b("a") #b("b") C #b("d") E #b("f"))
 (defun listize-parts
  ([() _] '())
  ([(x . xs) caps-parts] (cond
                          ; if x in caps-parts, don't turn it into a list.
                          ((: lists member x caps-parts)
                           (cons x (listize-parts xs caps-parts)))
                          ; else, turn x into a list and recurse.
                          ; NB: we convert to binary because LFE
                          ;     currently has a problem converting
                          ;     to lists.  It wants to execute them
                          ;     after they are constructed (it's not quoting).
                          (else
                           (cons (b x)
                            (listize-parts xs caps-parts)))))))

; (defkey rambo (a b C d E f)) generates equivalent of:
; rambo(C, E) -> eru:er_key(<<"a">>, <<"b">>, C, <<"d">>, E, <<"f">>).
; i.e. anything starting with caps is an argument and everything else
; is presented to the key generator as-is.
; also works with one arg: (defkey (site N)) makes: site:N(N)
(defmacro defkey
 ([parts] `(defkey ,(a (join-colon (ll parts))) ,parts))
 ([name parts] (let* ((variable-parts (extract-caps parts))
                      (adjusted-list-parts
                       (listize-parts parts variable-parts)))
   `(defun ,name ,variable-parts
     (: eru er_key ,@adjusted-list-parts)))))
