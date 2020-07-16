(import (chicken string) srfi-1)

(define-syntax ▽ (syntax-rules () ((_ . α) (define . α))))
(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))

; delay (f α) = (λ (ω) (f α ω))
(define-syntax delay
  (syntax-rules ()
    ((_ f ...) (lambda (α) (f ... α)))))

; application (f α) = (f α)
(define (application f α) (f α))

; function composition (f g α) = (f (g α))
(define-syntax delay?
  (syntax-rules (∘ λ Λ &&& *** ◁ ◀ ∴ ∵ ? ⇒ ⇐ ∈ ∀ ~)
    ((_ (∘ α ...)) (composition α ...))
    ((_ (λ α ...)) (lambda α ...))
    ((_ (Λ α ...)) (cut α ...))
    ((_ (&&& α ...)) (fanout α ...))
    ((_ (*** α ...)) (split-strong α ...))
    ((_ (◁ α ...)) (hook α ...))
    ((_ (◀ α ...)) (dyhook α ...))
    ((_ (∴ α ...)) (fork α ...))
    ((_ (∵ α ...)) (dyfork α ...))
    ((_ (? α ...)) (tacit-if α ...))
    ((_ (⇒ α ...)) (tacit-map α ...))
    ((_ (⇐ α ...)) (tacit-filter α ...))
    ((_ (∈ α ...)) (tacit-find α ...))
    ((_ (∀ α ...)) (tacit-for-each α ...))
    ((_ (~ α ...)) (flipping α ...))
    ((_ (f α ...)) (delay f α ...))
    ((_ f) (delay f))))

(define-syntax delay-params
  (syntax-rules ()
    ((_ (f ...)) (list (delay? (f ...))))
    ((_ f) (list (delay? f)))
    ((_ (f ...) g ...) (cons (delay? (f ...)) (delay-params g ...)))
    ((_ f g ...) (cons (delay? f) (delay-params g ...)))))

(define-syntax composition
  (syntax-rules ()
    ((_ f ...) (lambda (α) (foldr application α (delay-params f ...))))))

; fanout (f g α) = '((f α) (g α))
(define-syntax fanout
  (syntax-rules ()
    ((_ f ...) (lambda (α) (map (lambda (ω) (ω α)) (delay-params f ...))))))

; split strong (f g '(α ω)) = '((f α) (g ω))
(define-syntax split-strong
  (syntax-rules ()
    ((_ f ...) (lambda (α) (map application (delay-params f ...) α)))))

; monadic hook (f g α) = (f α (g α))
(define-syntax hook
  (syntax-rules ()
    ((_ (f ...) g ...) (lambda (α) ((composition (f ... α) g ...) α)))
    ((_ f g ...) (lambda (α) ((composition (f α) g ...) α)))))

; dyadic hook (f g α ω) = (f α (g ω))
(define-syntax dyhook
  (syntax-rules ()
    ((_ (f ...) g ...) (lambda (α ω) ((composition (f ... α) g ...) ω)))
    ((_ f g ...) (lambda (α ω) ((composition (f α) g ...) ω)))))

; monadic fork (f g h α) = (f (g α) (h α))
(define-syntax fork
  (syntax-rules ()
    ((_ (f ...) g ...) (lambda (α) ((composition (apply f ...) (&&& g ...)) α)))
    ((_ f g ...) (lambda (α) ((composition (apply f) (&&& g ...)) α)))))

; dyadic fork (f g h α ω) = (f (g α) (h ω))
(define-syntax dyfork
  (syntax-rules ()
    ((_ f g h ...) (lambda (α ω) ((∴ f (∘ g ↑) (∘ h ↓↑) ...) (list α ω))))))

; flip (f α ω) = (f ω α)
(define-syntax flipping
  (syntax-rules ()
    ((_ f α ...) (λ (ω) (f ω α ...)))))

; point-free if
(define-syntax tacit-if
  (syntax-rules ()
    ((_ p f g) (lambda (α) (if ((delay? p) α) ((delay? f) α) ((delay? g) α))))))

; point-free cond
(define-syntax match
  (syntax-rules (…)
    ((_ α (p f) ... (… g))
     (cond (((delay? p) α) ((delay? f) α)) ... (else ((delay? g) α))))
    ((_ α (p f) ...)
     (cond (((delay? p) α) ((delay? f) α)) ...))))

; general point-free constructor for higher order functions
(define-syntax tacit-f
  (syntax-rules ()
    ((_ f g α ...) (lambda (ω) (f (delay? g) α ... ω)))))

; point-free map, filter, find, for-each
(define-syntax tacit-map (syntax-rules () ((_ . α) (tacit-f map . α))))
(define-syntax tacit-filter (syntax-rules () ((_ . α) (tacit-f filter . α))))
(define-syntax tacit-find (syntax-rules () ((_ . α) (tacit-f find . α))))
(define-syntax tacit-for-each(syntax-rules () ((_ . α) (tacit-f for-each . α))))

; identity α = α
(define (id α) α)

; const (α ω) = α
(define (const α ω) α)

; uncurrying (f '(α ω) = (f α ω)
(define (uncurry f) (λ (α) (apply f α)))

; "and" as a normal function (AN)
(define (∧ α ω) (and α ω))

; "or" as a normal function (OR)
(define (∨ α ω) (or α ω))

; synonyms (poorly ordered functions will be flipped mercilessly)
(define-syntax ⊃ (syntax-rules () ((_ . α) (delay . α))))          ;)C
(define-syntax ∘ (syntax-rules () ((_ . α) (composition . α))))    ;Ob
(define-syntax ◁ (syntax-rules () ((_ . α) (hook . α))))           ;Tl
(define-syntax ◀ (syntax-rules () ((_ . α) (dyhook . α))))         ;PL
(define-syntax ∴ (syntax-rules () ((_ . α) (fork . α))))           ;.:
(define-syntax ∵ (syntax-rules () ((_ . α) (dyfork . α))))         ;:.
(define-syntax &&& (syntax-rules () ((_ . α) (fanout . α))))
(define-syntax *** (syntax-rules () ((_ . α) (split-strong . α))))
(define-syntax Λ (syntax-rules () ((_ . α) (cut . α))))            ;L*
(define-syntax ⇒ (syntax-rules () ((_ . α) (tacit-map . α))))      ;=>
(define-syntax ⇐ (syntax-rules () ((_ . α) (tacit-filter . α))))   ;<=
(define-syntax ∈ (syntax-rules () ((_ . α) (tacit-find . α))))     ;(-
(define-syntax ∀ (syntax-rules () ((_ . α) (tacit-for-each . α)))) ;FA
(define-syntax ? (syntax-rules () ((_ . α) (tacit-if . α))))
(define-syntax ~ (syntax-rules () ((_ . α) (flipping . α))))
(define ⊥ id)                                                      ;-T
(define ∞ const)                                                   ;00
(define ◇ append)                                                  ;Dw
(define ◆ conc)                                                    ;Db
(define s⊥□ string-split)                                          ;OS
(define □⊥s string-intersperse)
(define ≡ equal?)                                                  ;=3
(define ∅ '())                                                     ;/0
(define ↑ car)                                                     ;-!
(define ↑↑ caar)
(define ↑↑↑ caaar)
(define ↓ cdr)                                                     ;-v
(define ↓↓ cddr)
(define ↓↓↓ cddr)
(define ↓↑ cadr)
(define ↓↓↑ caddr)
(define ↓↓↓↑ cadddr)
(define ↓↑↑ caadr)
(define ↓↑↑↑ caaadr)
(define ↑. (flip take))
(define ↓. (flip drop))
(define ↓… last)
(define ⊇ uncurry)                                                 ;)_
(define ∷ cons)                                                    ;::
(define → foldr)                                                   ;->
(define ← foldl)                                                   ;<-
(define ∋ (flip member))                                           ;)-
(define ι iota)                                                    ;i*
(define ρ length)                                                  ;r*
(define ⌐ flatten)                                                 ;NI
(define ⇔ reverse)                                                 ;==
(define ∂ assoc)                                                   ;dP
(define $ apply)
(define v⊥x vector->list)
(define x⊥v list->vector)
(define s⊥x string->list)
(define x⊥s list->string)
(define v? vector?)
(define p? pair?)
(define ∅? null?)
