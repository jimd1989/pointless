(define-syntax λ (syntax-rules () ((_ . α) (lambda . α))))

; monadic hook (f g α) = (f α (g α))
(define-syntax hook
  (syntax-rules ()
    ((_ f g ...) (lambda (α) (f α (g α) ...)))))

; dyadic hook (f g α ω) = (f α (g ω))
(define-syntax dyhook
  (syntax-rules ()
    ((_ f g ...) (lambda (α ω) (f α (g ω) ...)))))

; monadic fork (f g h α) = (f (g α) (h α))
(define-syntax fork
  (syntax-rules ()
    ((_ f g ...) (lambda (α) (f (g α) ...)))))

; dyadic fork (f g h α ω) = (f (g α) (h ω))
(define-syntax dyfork
  (syntax-rules ()
    ((_ f g h ...) (lambda (α ω) (f (g α) (h ω) ...)))))

; fanout (f g α) = '((f α) (g α))
(define-syntax fanout
  (syntax-rules ()
    ((_ f ...) (lambda (α) (list (f α) ...)))))

; identity α = α
(define (id α) α)

; const (α ω) = α
(define (const α) (λ (ω) α))

; application (f α) = (f α)
(define (application f α) (f α))

; split strong (f g '(α ω)) = '((f α) (g ω))
(define (split-strong . fs)
  (λ (α) (map application fs α)))

; function composition (f g α) = (f (g α))
(define (composition . fs)
  (λ (α) (foldr application α fs)))

; point-free cond
(define-syntax match
  (syntax-rules (otherwise)
    ((_ α (p f) ... (otherwise g)) (cond ((p α) (f α)) ... (else (g α))))
    ((_ α (p f) ...) (cond ((p α) (f α)) ...))))

; synonyms
(define-syntax ◁ (syntax-rules () ((_ . α) (hook . α))))    ;Tl
(define-syntax ◀ (syntax-rules () ((_ . α) (dyhook . α))))  ;PL
(define-syntax ∴ (syntax-rules () ((_ . α) (fork . α))))    ;.:
(define-syntax ∵ (syntax-rules () ((_ . α) (dyfork . α))))  ;:.
(define-syntax &&& (syntax-rules () ((_ . α) (fanout . α))))
(define-syntax ⊂ (syntax-rules () ((_ . α) (cut . α))))     ;(C
(define ≡ id)                                               ;=3
(define ∞ const)                                            ;00
(define ∘ composition)                                      ;Ob
(define ◇ append)                                           ;Dw
(define ∅ '())                                              ;/0
(define *** split-strong)
