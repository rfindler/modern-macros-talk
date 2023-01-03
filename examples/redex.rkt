#lang racket
(require redex)

(define-language L

  (e ::= (e e) x (λ (x : τ) e))
  (τ ::= ι (-> τ τ))

  (Γ ::= · (+ x τ Γ))
  
  (x ::= variable-not-otherwise-mentioned))

  
(define-judgment-form L
  #:contract (⊢ Γ e τ)
  #:mode (⊢ I I O)

  [(⊢ Γ e_1 (-> τ_2 τ_3))   (⊢ Γ e_2 τ_2)
   --------------------------------------
   (⊢ Γ (e_1 e_2) τ_3)]

  [(⊢ (+ x τ_1 Γ) e τ_2)
   -------------------------------
   (⊢ Γ (λ (x : τ_1) e) (-> τ_1 τ_2))]

  [(⊢ Γ x_1 τ_1)   (where #true (≠ x_1 x_2))
   -----------------------------------------
   (⊢ (+ x_2 τ_2 Γ) x_1 τ_1)]


  [-----------------------
   (⊢ (+ x_1 τ_1 Γ) x_1 τ_1)])

(define-metafunction L
  ≠ : x x -> boolean
  [(≠ x x) #false]
  [(≠ x_1 x_2) #true])


(test-judgment-holds (⊢ · (λ (x : ι) x) (-> ι ι)))
(test-judgment-holds (⊢ · (λ (y : ι) ((λ (x : ι) x) y)) (-> ι ι)))
(test-judgment-holds (⊢ · (λ (x : ι) (λ (f : (-> ι ι)) (f x))) (-> ι (-> (-> ι ι) ι))))
(test-judgment-holds (⊢ · (λ (x : ι) (λ (x : (-> ι ι)) x)) (-> ι (-> (-> ι ι) (-> ι ι)))))
(test-judgment-holds (⊢ · (λ (x : ι) (λ (y : (-> ι ι)) x)) (-> ι (-> (-> ι ι) ι))))

(with-compound-rewriters
    (['⊢ (λ (lws) (list (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4)))]
     ['≠ (λ (lws) (list (list-ref lws 2) "≠" (list-ref lws 3)))]
     ['-> (λ (lws) (list "" (list-ref lws 2) " → " (list-ref lws 3)))]
     ['+ (λ (lws) (list "{" (list-ref lws 2) ":" (list-ref lws 3) "} ⊕ " (list-ref lws 4)))])
  (render-judgment-form ⊢))

(generate-term L #:satisfying (⊢ · e τ) 3)
