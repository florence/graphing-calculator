#lang racket/gui
(require "graph.rkt" (only-in plot plot-new-window?))

(provide main)
(define (main) (show))

;; -> Void
;; Show a new graphing calculator 
(define (show)
  ;; main frame
  (define frame (new frame% [label "A Basic Calculator"]))
  ;; input fields
  (define graph (make-grapher frame))
  ;; the button
  (define go!
    (new button% [parent frame]
         [label "Graph It!"]
         [callback 
          (lambda (button event)
            (with-handlers ([exn:fail? show-error-dialog])
              (render (graph))))]))
  ;; rendering
  (define render (make-renderer frame))
  ;; go
  (send frame show #t))

(define choices-names '("normal" "parametric" "polar"))
(define choices-functions (list graph2d parametric2d polar2d))
(define choices-labels '(("f(x) = ") ("x(t) = " "y(t) = ") ("r(t) = ")))

;; parentable -> (-> image-snip%)
;; take a parent object attached the fields needed for getting user input
;; and return a function for generating an image-snip% from user input
(define (make-grapher parent)
  (define container (new pane% [parent parent]))
  (define min (new text-field% [label "min"] [parent parent] [init-value "-10"]))
  (define max (new text-field% [label "max"] [parent parent] [init-value "10"]))
  (define inverse? (new check-box% [label "invert?"] [parent parent]))
  
  (define (fields* . strs) 
    (call-with-values (λ () (apply function-fields container strs)) list))
  
  (define choices-map
    (for/hash ([n choices-names] [f choices-functions] [l choices-labels])
      (values n (cons f (apply fields* l)))))
  
  (define graph void)
  (define deactive void)
  (define (update-to! s)
    (define-values (grapher activate getter) (apply values (hash-ref choices-map s)))
    (deactive)
    (activate)
    (set! deactive activate)
    (set! graph (λ (min max inv?) (apply grapher min max inv? (getter)))))
  
  (define chooser
    (new choice% [parent parent] [label "function type"] [choices choices-names]
         [callback (λ (c _) (update-to! (send c get-string-selection)))]))
  
  (update-to! (send chooser get-string-selection))
  
  (λ ()
    (graph (string->number (send min get-value))
           (string->number (send max get-value))
           (send inverse? get-value))))

;; parentable string... -> (->) (-> (listof strings))
;; add one text field to the parent for each name (using the given name as the label)
;; the fields are hidden
;; return a function to show/hide the fields and a function to get values from all of them
(define (function-fields parent . names)
  (define p (new horizontal-panel% [parent parent] [style '(deleted)]))
  (define functions (for/list ([name names]) (new text-field% [label name] [parent p])))
  (values
   (let ([? #t])
     (λ () (if ? (send parent add-child p) (send parent delete-child p)) 
       (set! ? (not ?))))
   (λ () (map (λ (f) (send f get-value)) functions))))

;; parentable -> (image-snip% ->)
;; add a canvas to the parent
;; return a function that will render an image-snip% to the canvas
(define (make-renderer parent)
  (define paste (new (class pasteboard%
                       (super-new)
                       (define/augment (can-interactive-move? e) #f)
                       (define/augment (can-interactive-resize? e) #f)
                       (define/augment (can-select? n ?) #f))))
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
  (define dialog (instantiate dialog% ("Error")))
  (new message% [parent dialog] [label (exn->label-string e)])
  (define panel (new horizontal-panel% [parent dialog] [alignment '(center center)]))
  (new button% [parent panel] [label "Ok"] [callback (λ _ (send dialog show #f))])
  
  (send dialog show #t))

;; exn? -> label-string?
;; returns first 200 chars of the exn's message
(define (exn->label-string e)
  (define s (~a (exn-message e)))
  (if (<= (string-length s) 200)
      s
      (substring s 0 200)))