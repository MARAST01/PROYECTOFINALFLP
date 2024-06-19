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

; Evaluar Programa
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp) (evaluar-expresion exp ambiente-inicial))
      )
    )
  )

; Evaluar Expresion
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      ;Numeros
      (num-exp (dato) dato)
      
      ;Identificadores
      (var-exp (id) (apply-env amb id))
      
      ;Strings
      (cadena-exp (str _) (substring str 1 (-(string-length str) 1)));pendiriente de revision porque lo dio copilot
      
      ;Valores Booleanos
      (prim-bool-exp (prim-bool exp) )
      
      ;Condicionales "If"
      ;se evalua la condicion en el ambiente, y se guarda como condicion-val
      ;posteriormente se evalua si su resultado fue un bool, si lo fue, se aplica un if de racket
      ;para retornar el valor de la consecuencia o el valor del else, ambos evaluados para poder ver su valor
      (if-exp (condicion consecuencia else)
              (let (
                    [condicion-val (evaluar-expresion condicion amb)]
                    )
                (if (boolean? condicion-val)
                    (if condicion-val (evaluar-expresion consecuencia amb) (evaluar-expresion else amb))
                    (eopl:error "Se esperaba un valor booleano para el IF"))
                )
              )

      ;Ligaduras locales
      (let-exp (ids rands body)
               (let
                   (
                    [lvalues (map (lambda (x) (evaluar-expresion x amb)) rands)]
                    )
                 (evaluar-expresion body (ambiente-extendido ids lvalues amb))
                 )
               )
      ;Primitivas
      ;si la primera exp es nula, significa que es una primitiva de una sola expresion, por ejemplo (add1 2)
      ;sino lo es, aplicamos el procedimiento que retorna evaluar-pimitiva a la expresion evaluada en el ambiente actual
      ;junto con la segunda expresion evaluada para generar la recursion
      (prim-exp (exp1 prim exp2)
                (if (null? exp1)
                    ;los casos en los que la primitiva requiere solamente un argumento
                    ((evaluar-primitiva prim) (evaluar-expresion exp2 amb) )
                    (if (= (length exp1) 1)
                        ;los casos en los que la primitiva requiere 2 o mas
                        
                        ;(display "estoy aqui")
                        ((evaluar-primitiva prim) (evaluar-expresion (car exp1) amb) (evaluar-expresion exp2 amb) )
                        
                        
                        ;en el caso en el que se envien mas expresiones de las esperadas entre una sola primitiva
                        ;ej (1 2 + 1)
                        (eopl:error "Se esperaba unicamente un segundo parametro para la operacion primitiva")
                        )
                    )
                )
      
      ;Procedimientos
      ;Esto ya que cuando se crea un procedimiento se crea un nuevo ambiente en una cerradura(closure) por lo cual
      ;previamente a esto creamos el datatype necesario para que sea reconocido.
      ;Por lo tanto la propia clausura, guarda el propio ambiente donde fue creado.
      (proc-exp (ids body)(closure ids body amb))
      
      ;Llamado a procedimiento
      (app-exp (rator rands)
               (let
                   ;area de definiciones
                   ;lrands = evaluamos cada una de los rands que serian los identificadores del procedimiento para
                   ;conocer sus valores, o eventualmente el valor que tomen al evaluarse
                   ;proc = evaluamos el rator que seria el nombre del procedimiento, para verificar que efectivamente lo sea
                   ([lrands (map (lambda (x) (evaluar-expresion x amb)) rands)]
                    [proc (evaluar-expresion rator amb)])
                 (if
                  ;si proc es un procedimiento, entramos a verificar
                  (procval? proc)
                  (cases procval proc
                    (closure (lid body old-env)
                             ;si el numero de variables ingresadas coniciden con las esperadas
                             ;si lo hace, evaluamos el cuerpo de el closure, osea del proc, con las variables
                             ;que estamos ingresando, ademas de el ambiente anterior
                             ;si no lo son simplemente lo retornamos en un mensaje de error
                             (if (= (length lid) (length lrands))
                                 (evaluar-expresion body (ambiente-extendido lid lrands old-env))
                                 (eopl:error "Se espearaban " (length lid) "parametros y se han recibido " (length lrands)))))
                  (eopl:error proc "No corresponde a un procedimiento") 
                  )))
      
      ;Begin
      ;
      (begin-exp (exp lexp)
                 (if
                  (null? lexp)
                  (evaluar-expresion exp amb)
                  (begin
                    (evaluar-expresion exp amb)
                    (letrec
                        ([evaluar-begin (lambda (lexp)
                                          (cond
                                            [(null? (cdr lexp)) (evaluar-expresion (car lexp) amb)]
                                            [else
                                             (begin
                                               (evaluar-expresion (car lexp) amb)
                                               (evaluar-begin (cdr lexp)))]
                                            ))])                         
                      (evaluar-begin lexp)))))
      ;;set
      (set-exp (id exp)
               (begin
                 (setref! (apply-env-ref amb id) (evaluar-expresion exp amb))
                 1))

      ;;For
      (for-exp (exp1 exp2 exp3 body)
               (cases expresion exp1
                 (for-assign (id exp)
                            (letrec
                             ;area definiciones
                                ;value = la expresion de la derecha a la asignacion para sacar su valor
                             ([value (evaluar-expresion exp amb)]
                                ;ambiente = el ambiente actual lo incorporamos a un ambiente que contiene
                                ;la variable de control, para poderla cambiar recursivamente y tomarla como
                                ;contador
                              [ambiente (ambiente-extendido (list id) (list value) amb)]
                                ;iterar = momento donde se aplica la exp3, osea la primitiva sobre el contador
                                ;mientras que la exp2, la condicion del for, sea true, ejecutamos sencuencialmente
                                ;con ayuda de racket, el body, y la primitiva sobre el contador para que cambie
                                ;en la siguiente iteracion
                              [iterar (lambda ()
                                        (if (evaluar-expresion exp2 ambiente)
                                            (begin
                                              (evaluar-expresion body ambiente)
                                              (evaluar-expresion exp3 ambiente)
                                              (iterar))
                                            ;retornamos un 1 por motivos practicos, cuando la condicion ya no es verdadera
                                            1)
                                        )
                                      ])
                              ;area de ejecucion
                              (iterar)
                              )
                            )
                 (else "El primer parametro de un for debe ser la asignacion de una variable")
                 )
               )
      
      ;While
      ;para el while la logica es muy parecida que la del for, solamente que no hay expresion contador,
      ;ni tampoco hay una expresion directa para cambiar el contador, debe ser especificada dentro del body
      (while-exp (exp1 body)
                 (letrec
                     ;area de definiciones
                     ;iterar = mientras que la exp1 sea verdadera en el ambiente actual, se ejecuta el cuerpo del while
                     ;cuando ya deja de serlo, simplemente retornamos un 1
                     ([iterar (lambda ()
                                (if (evaluar-expresion exp1 amb)
                                    (begin
                                      (evaluar-expresion body amb)
                                      (iterar))
                                    1)
                                )])
                   ;area de ejecucion
                   (iterar)))


      ;Estructuras
      ;Create Struct
      (create-exp (structId id lids exp)
                  (evaluar-expresion exp (ambiente-extendido
                                          (list structId)
                                          (list (a-struct (cons id lids))) amb)))

      ;struct-instance
      (struct-instance (id lexp)
                       (let (
                             [estructura (apply-env amb id)]
                             [lvalues (map (lambda (x) (evaluar-expresion x amb)) lexp)]
                             )
                         (begin
                           (cases struct estructura
                             (a-struct (lids) (ambiente-extendido lids lvalues (ambiente-vacio)))))
                         ))

      ;Acceso
      (access-exp (id varId)
                  (let (
                        [estructura (apply-env amb id)]
                        )
                    (apply-env estructura varId)
                    ))

      ;Modificacion
      (edit-exp (id varId exp)
                (begin
                 (setref! (apply-env-ref (apply-env amb id) varId) (evaluar-expresion exp amb))
                 1))

      
      ;prevemos excepciones (otros casos)
      (else exp) 
      )
    )
  )

;;Primitivas (Retorna un metodo)
(define evaluar-primitiva
  (lambda (prim)
    (cases primitiva prim
      ;;Primitivas aritmeticas
      (sum-prim () (lambda (a b)(+ a b)))
      (minus-prim () (lambda (a b)(- a b)))
      (mult-prim () (lambda (a b)(* a b)))
      (div-prim () (lambda (a b)(/ a b)))
      (mod-prim () (lambda (a b)(modulo a b)))
      (add-prim () (lambda (a)(+ a 1)))
      (sub-prim () (lambda (a)(- a 1)))
      ;;primitivas booleanas
      (mayor-prim ()(lambda (a b)(> a b)))
      (mayorigual-prim ()(lambda (a b)(>= a b)))
      (menor-prim ()(lambda (a b)(< a b)))
      (menorigual-prim ()(lambda (a b)(<= a b)))
      (igual-prim ()(lambda (a b)(= a b)))
      ;;primitivas sobre cadenas
      (len-prim ()(lambda (a)(string-length a)))
      (concat-prim ()(lambda (a b)(string-append a b)))
      )
    )
  )
;; ambientes
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))

(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))

;Struct
;tipo de dato estructura, para poder manejar los cases por separado y tomarlo como tipo abstracto de dato
(define-datatype struct struct?
  (a-struct (l list?)))

;;;Ambiente inicial
(define ambiente-inicial
  (ambiente-extendido '(x y z) '(0 0 0)
                      (ambiente-extendido '(a b c) '(4 5 6)
                                          (ambiente-vacio))))



(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))



;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm)  pgm)
    (sllgen:make-stream-parser 
      lexica
      gramatica)))


(interpretador)