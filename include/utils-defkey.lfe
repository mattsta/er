(eval-when-compile
 ; make-fns-from-args consolidates double-defkey for defkey recursive uses
 (defun make-functions-from-args (parts)
  `(progn
    (defkey ,(a (join-under (ll parts))) ,parts)
    (defkey ,(a (join-colon (ll parts))) ,parts)))
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
; also works with one arg: (defkey (site N)) makes: site:N(N) and site_N(N)
; counter keys auto-generate counter accessors:
; (defkey (counter bob)) => counter:bob(), counter_bob(), bob:N(N), bob_N(N)
; normal usage of defining a function name and its components:
; (defkey zomboid (rabbit Hole)) => zomboid(Hole) = rabbit:[Hole]
; normal usage of auto-making function name based on component names:
; (defkey (rabbit Hole)) => rabbit(Hole) = rabbit:Hole
(defmacro defkey
 ([('counter . (name))] `(progn
                          ,(make-functions-from-args (list 'counter name))
                          (defkey (,name N))))
 ([parts] (when (is_list parts)) (make-functions-from-args parts))
 ([name parts] (let* ((variable-parts (extract-caps parts))
                      (adjusted-list-parts
                       (listize-parts parts variable-parts)))
   `(defun ,name ,variable-parts
     (: eru er_key ,@adjusted-list-parts)))))

; (defkey-suite post (to tags last_update authors (comment Comment))) creates:
; counter:post(), counter_post()
; post:N:to(N), post_N_to(N)
; post:N:tags(N), post_N_tags(N)
; post:N:last_update(N), post_N_last_update(N)
; post:N:authors(N), post_N_authors(N)
; post:N:comment:Comment(N, Comment), post_N_comment_Comment(N, Comment)
(defmacro defkey-suite (key subkeys)
 `(progn
   (defkey (counter ,key))
   ,@(: lists map
      (match-lambda
       ([subkey] (when (is_atom subkey)) `(defkey (,key N ,subkey)))
       ([subkey] (when (is_list subkey)) `(defkey (,key N ,@subkey))))
      subkeys)))
