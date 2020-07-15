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
(define-syntax delegate-delay
  (syntax-rules (∘ λ Λ &&& *** ◁ ◀ ⇒)
    ((_ (∘ α ...)) (composition α ...))
    ((_ (λ α ...)) (lambda α ...))
    ((_ (Λ α ...)) (cut α ...))
    ((_ (&&& α ...)) (fanout α ...))
    ((_ (*** α ...)) (split-strong α ...))
    ((_ (◁ α ...)) (hook α ...))
    ((_ (◀ α ...)) (dyhook α ...))
    ((_ (⇒ α ...)) (macro-map α ...))
    ((_ (f α ...)) (delay f α ...))
    ((_ f) (delay f))))

(define-syntax delay-params
  (syntax-rules ()
    ((_ (f ...)) (list (delegate-delay (f ...))))
    ((_ f) (list (delegate-delay f)))
    ((_ (f ...) g ...) (cons (delegate-delay (f ...)) (delay-params g ...)))
    ((_ f g ...) (cons (delegate-delay f) (delay-params g ...)))))

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
    ((_ f g ...) (lambda (α) ((composition (f α) g ...) α)))))

; dyadic hook (f g α ω) = (f α (g ω))
(define-syntax dyhook
  (syntax-rules ()
    ((_ f g ...) (lambda (α ω) ((composition (f α) g ...) ω)))))

; monadic fork (f g h α) = (f (g α) (h α))
(define-syntax fork
  (syntax-rules ()
    ((_ f g ...) (lambda (α) (f (g α) ...)))))

; dyadic fork (f g h α ω) = (f (g α) (h ω))
(define-syntax dyfork
  (syntax-rules ()
    ((_ f g h ...) (lambda (α ω) (f (g α) (h ω) ...)))))

; identity α = α
(define (id α) α)

; const (α ω) = α
(define (const α) (λ (ω) α))

; uncurrying (f '(α ω) = (f α ω)
(define (uncurry f) (λ (α) (apply f α)))

; "and" as a normal function
(define (& α ω) (and α ω))

; point-free map
(define-syntax macro-map
  (syntax-rules ()
    ((_ (f ...)) (lambda (α) (map (delegate-delay (f ...)) α)))
    ((_ f) (lambda (α) (map (delegate-delay f) α)))))

; point-free if
(define (? p f g) (λ (α) (if (p α) (f α) (g α))))

; point-free cond
(define-syntax match
  (syntax-rules (otherwise)
    ((_ α (p f) ... (otherwise g)) (cond ((p α) (f α)) ... (else (g α))))
    ((_ α (p f) ...) (cond ((p α) (f α)) ...))))

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
(define-syntax ⇒ (syntax-rules () ((_ . α) (macro-map . α))))      ;=>
(define ⊥ id)                                                      ;-T
(define ∞ const)                                                   ;00
(define ◇ append)                                                  ;Dw
(define ◆ conc)                                                    ;Db
(define .◆ string-split)
(define ◆. string-intersperse)
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
(define ⊇ uncurry)                                                 ;)_
(define ⇐ filter)                                                  ;<=
(define → foldr)                                                   ;->
(define ← foldl)                                                   ;<-
(define ∈ find)                                                    ;(-
(define ∋ (flip member))                                           ;)-
(define ∀ for-each)                                                ;FA
(define ι iota)                                                    ;i*
(define ρ length)                                                  ;r*
(define ⌐ flatten)                                                 ;NI
(define ⇔ reverse)                                                 ;==
(define $ apply)
(define ~ flip)
(define v⊥x vector->list)
(define x⊥v list->vector)
(define s⊥x string->list)
(define x⊥s list->string)
