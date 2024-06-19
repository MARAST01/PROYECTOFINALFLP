#lang eopl

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
   ;Borrar  
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string) 
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

(define gramatica
  '(
    (programa (expresion) a-program)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)    
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    )
  )
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (a-program (body)
                 (evaluar-expresion body (init-env)))
    )
  )
)
; Evaluar Expresion
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (bool-exp (bool) (
        cases bool-expresion bool
        (true-exp () #t)
        (false-exp () #f)
      ))
      (var-exp (id) (apply-env env id))
      (num-exp (num) (
        cases numero-exp num
        (decimal-num (num) num)
        (bin-num (num) (string->symbol num))
        (octal-num (num) (string->symbol num))
        (hex-num (num) (string->symbol num))
        (float-num (num) num)
      ))
      (cadena-exp (id lids) (
        let loop( [values lids]
                  [acc (symbol->string id)]
                )
                (cond
                  [(null? values) acc]
                  [else (loop (cdr values) (string-append acc " " (symbol->string (car values))))])
      ))
      (decl-exp (decl) (
        cases var-decl decl
        (lvar-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body
                                  (extend-env ids args env))))

        ;;;Revisar el Let para que no acepte set
        (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body
                                  (extend-env ids args env))))
      ))
      (lista-exp (lexp) (map (lambda (exp) (evaluar-expresion exp env)) lexp))
      (cons-exp (exp1 exp2) (cons (evaluar-expresion exp1 env) (evaluar-expresion exp2 env)))
      (empty-list-exp () '())
      (array-exp (lexp) (list->vector (map (lambda (exp) (evaluar-expresion exp env)) lexp)))
      
     
     ;; Revisar set
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (evaluar-expresion rhs-exp env))
                 1))
      (prim-num-exp (exp1 prim exp2) '())
      (prim-bool-exp (prim lexp) (
        cases primitivaBooleana prim
        (and-prim () (let loop([values (map (lambda (exp) (evaluar-expresion exp env)) lexp)])
                        (cond
                          ((null? values) #t)
                          ((null? (cdr values)) (car values))
                          (else (and (car values) (loop (cdr values))))))
                      )
        (or-prim () (let loop([values (map (lambda (exp) (evaluar-expresion exp env)) lexp)])
                       (cond
                         ((null? values) #f)
                         ((null? (cdr values)) (car values))
                         (else (or (car values) (loop (cdr values))))))
                     )
        (xor-prim () (let ([a (evaluar-expresion (car) env)]
                           [b (evaluar-expresion (cadr) env)]) 
                          (and (or a b) (not (and a b)))
                    ))
        (not-prim () (not (evaluar-expresion (car lexp) env)))
      ))
      (prim-list-exp (prim exp) (
        cases primitivaListas prim
        (first-primList () (car (evaluar-expresion exp env)))
        (rest-primList () (cdr (evaluar-expresion exp env)))
        (empty-primList () (null? (evaluar-expresion exp env)))
      ))
      (prim-array-exp (prim lexp) (
        cases primitivaArray prim
        (length-primArr () (vector-length (evaluar-expresion exp env)))
        (index-primArr () (vector-ref (evaluar-expresion (car lexp) env) (evaluar-expresion (cadr lexp) env)))
        (slice-primArr () '())
        (setlist-primArr () '()) ))
      (prim-cad-exp (prim lexp) (
        cases primitivaCadena prim
        (concat-primCad () (
          let loop( [values (map (lambda (exp) (evaluar-expresion exp env)) lexp)]
                    [acc ""]
                  )
                  (cond
                    [(null? values) acc]
                    [else (loop (cdr values) (string-append acc (car values)))]
          )
        ))
        (length-primCad () (string-length (evaluar-expresion (car lexp) env)))
        (index-primCad () (string-ref (evaluar-expresion (car lexp) env) (evaluar-expresion (cadr lexp) env)))
      ))
      (if-exp (test-exp true-exp false-exp)
              (if (evaluar-expresion test-exp env)
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env)))

      (begin-exp (exp exps) 
                 (let loop ((acc (evaluar-expresion exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (evaluar-expresion (car exps) 
                                               env)
                              (cdr exps)))))



      (for-exp (id from-exp until-exp by-exp do-exp) '())
      (while-exp (exp1 exp2) '())
      (switch-exp (exp lexp1 lexp2  default-exp) '())
      (func-exp (lids exp) '())
      (call-exp (exp lexp) '())
      (new-struct-exp (id lexp) '())
      (get-struct-exp (exp id ) '())
      (set-struct-exp (exp1 id exp2) '())
      (match-exp (exp1 lregular-exp exp2) '())
    ))
)



;Struct
;tipo de dato estructura, para poder manejar los cases por separado y tomarlo como tipo abstracto de dato
(define-datatype struct struct?
  (a-struct (l list?)))

;;;Ambiente inicial




(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))



;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser 
      lexica
      gramatica)))





;;; Ambiente


(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))
;; Ambientes


;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))


;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


(interpretador)