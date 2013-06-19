#lang racket/gui
(require "graph.rkt" (only-in plot plot-new-window?))

(provide main)
(define (main) (show))

(define (show)
  ;; main frame
  (define frame
    (new frame% [label "A Basic Calculator"]))
  ;; input fields
  (define-values (get graph) (make-grapher frame))
  (define min (new text-field% [label "min"] [parent frame] [init-value "-10"]))
  (define max (new text-field% [label "max"] [parent frame] [init-value "10"]))
  (define inverse?
    (new check-box%	 
         [label "invert?"]	 
         [parent frame]))
  ;; rendering
  (define render (make-renderer frame))
  ;; the button
  (define go!
    (new button% [parent frame]
         [label "Graph It!"]
         [callback 
          (lambda (button event)
            (with-handlers ([exn:fail? show-error-dialog])
              (render
               (apply graph
                      (string->number (send min get-value))
                      (string->number (send max get-value))
                      (send inverse? get-value)
                      (get)))))]))
  ;; go
  (send frame show #t))

;; parentable -> grapher (-> string)
(define (make-grapher parent)
  (define (fields* . str) 
    (call-with-values (λ () (apply function-fields container str)) list))
  
  (define container (new pane% [parent parent]))
  (define choices
    (hash "normal" (cons graph2d (fields* "f(x) = "))
          "parametric" (cons parametric2d (fields* "x(t) = " "y(t) = "))
          "polar" (cons polar2d (fields* "r(t) = "))))
  
  (define getb (box void))
  (define graphb (box void))
  (define deactiveb (box void))
  (define (update-to! s)
    (define-values (grapher e getter) (apply values (hash-ref choices s)))
    ((unbox deactiveb) #f)
    (e #t)
    (set-box! deactiveb e)
    (set-box! getb getter)
    (set-box! graphb grapher))
  
  (define chooser
    (new choice% [parent parent] [label "function type"] [choices (hash-keys choices)]
         [callback (λ (c _) (update-to! (send c get-string-selection)))]))
  (update-to! (send chooser get-string-selection))
  (values (λ _ (apply (unbox getb) _)) (λ _ (apply (unbox graphb) _))))

;; parentable string... -> (bool ->) (-> (listof strings))
(define (function-fields parent . names)
  (define p (new horizontal-panel% [parent parent] [style '(deleted)]))
  (define functions (map (λ (name) (new text-field% [label name] [parent p])) names))
  (values
   (λ (?) (if ? (send parent add-child p) (send parent delete-child p)))
   (λ () (map (λ (f) (send f get-value)) functions))))

;; parentable -> (image-snip% ->)
(define (make-renderer parent)
  (define paste  (new pasteboard%))
  (define canvas (new editor-canvas%
                      [parent parent]
                      [editor paste]
                      [min-width 430]	 
                      [min-height 420]	 
                      [stretchable-width #f]	 
                      [stretchable-height #f]
                      [style '(no-hscroll no-vscroll)]))
  (λ (pic)
    (send paste begin-edit-sequence)
    (send paste select-all)
    (send paste clear)
    (send paste insert pic 0 0)
    (send paste end-edit-sequence)))
  
;; -> (Nat Board -> Void)
;; display current state in a canvas
(define (show-error-dialog e)
  ; Create a dialog
  (define dialog (instantiate dialog% ("Error")))
  
  ; Add a text field to the dialog
  (new message% [parent dialog] [label (exn->label-string e)])
  
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel (new horizontal-panel% [parent dialog]
                     [alignment '(center center)]))
  
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel] [label "Ok"]
       [callback (λ _ (send dialog show #f))])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))
  
  ; Show the dialog
  (send dialog show #t))

(define (exn->label-string e)
  (define s (~a (exn-message e)))
  (if (<= (string-length s) 200)
      s
      (substring s 0 200)))