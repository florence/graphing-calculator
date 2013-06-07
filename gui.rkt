#lang racket/gui
(require "graph.rkt" (only-in plot plot-new-window?))

(define (show)
  ;; main frame
  (define frame
    (new frame% [label "A Basic Calculator"]))
  ;; input fields
  (define function (new text-field% [label "f(x) = "] [parent frame]))
  (define min (new text-field% [label "min"] [parent frame] [init-value "-10"]))
  (define max (new text-field% [label "max"] [parent frame] [init-value "10"]))
  (define inverse?
    (new check-box%	 
         [label "invert?"]	 
         [parent frame]))
  ;; rendering
  (define paste  (new pasteboard%))
  (define canvas (new editor-canvas%
                      [parent frame]
                      [editor paste]
                      [min-width 430]	 
                      [min-height 420]	 
                      [stretchable-width #f]	 
                      [stretchable-height #f]
                      [style '(no-hscroll no-vscroll)]))
  (define (render pic)
    (send paste begin-edit-sequence)
    (send paste select-all)
    (send paste clear)
    (send paste insert pic 0 0)
    (send paste end-edit-sequence))
  ;; the button
  (define go!
    (new button% [parent frame]
         [label "Graph It!"]
         [callback 
          (lambda (button event)
            (with-handlers ([exn:fail? show-error-dialog])
              (render
               (graph2d (send function get-value)
                        (string->number (send min get-value))
                        (string->number (send max get-value))
                        (send inverse? get-value)))))]))
  ;; go
  (send frame show #t))
  
;; -> (Nat Board -> Void)
;; display current state in a canvas
(define (show-error-dialog e)
  ; Create a dialog
  (define dialog (instantiate dialog% ("Error")))
  
  ; Add a text field to the dialog
  (new message% [parent dialog] [label (exn-message e)])
  
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