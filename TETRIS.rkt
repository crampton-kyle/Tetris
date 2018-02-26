;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TETRIS) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
 
;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))
;; where the x and y is the lower right corner in grid coordinates
 
;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))
 
;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.
 
;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphical Constants

(define BLOCK-WIDTH 20)
(define BLOCK-HEIGHT BLOCK-WIDTH)

(define NUMBER-OF-ROWS 20)
(define NUMBER-OF-COLUMNS 10)

(define WIDTH (* BLOCK-WIDTH NUMBER-OF-COLUMNS))
(define HEIGHT (* BLOCK-HEIGHT NUMBER-OF-ROWS))
(define DEFAULT-BLOCK (rectangle BLOCK-WIDTH BLOCK-HEIGHT 'outline 'black))
(define BG (empty-scene WIDTH HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples

(define BLOCK-SET1 (list (make-block 1 20 'blue)
                         (make-block 2 20 'blue)
                         (make-block 3 20 'blue)
                         (make-block 4 20 'blue)))

(define TETRA1 (make-tetra (make-posn 1.5 1.5) (list (make-block 1 1 'green)
                                                     (make-block 2 1 'green)
                                                     (make-block 1 2 'green)
                                                     (make-block 2 2 'green))))

(define INITIAL-WORLD (make-world TETRA1 '()))
(define WORLD1 (make-world TETRA1 BLOCK-SET1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Function

(define (main who-gives-a-fuck)
  (big-bang INITIAL-WORLD
            [to-draw render]
            [on-key shift-tetra]
            [on-tick update-world 1]
            [stop-when bricks-overlap? (λ (w) (overlay (text "HAHA YOU LOSE" 30 'black) BG))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to-draw

;; World -> Image
;; draws all tetra onto the screen
(define (render w)
  (overlay-lines (draw-tetra (world-tetra w)             
                             (foldr draw-block BG (world-pile w)))))

;; Tetra Image -> Image
;; draws the tetra onto the image
(define (draw-tetra t i)
  (foldr draw-block i (tetra-blocks t)))

;; Block Image -> Image
;; draws the block onto the image
(define (draw-block b i)
  (local [;; Image of block
          (define block-image (rectangle BLOCK-WIDTH BLOCK-HEIGHT
                                         'solid (block-color b)))]    
    (place-image block-image
                 (update-coord (block-x b) BLOCK-WIDTH)
                 (update-coord (block-y b) BLOCK-HEIGHT)
                 i)))

;; Image -> Image
;; overlays a grid onto the screen
(define (overlay-lines i)
  (local [(define (draw-rows i n)
            (cond
              [(zero? n) i]
              [else (draw-rows (scene+line i
                                           0
                                           (* n BLOCK-HEIGHT)
                                           WIDTH
                                           (* n BLOCK-HEIGHT)
                                           'black) (sub1 n))]))
          (define (draw-columns i n)
            (cond
              [(zero? n) i]
              [else (draw-columns (scene+line i
                                              (* n BLOCK-WIDTH)
                                              0
                                              (* n BLOCK-WIDTH)
                                              HEIGHT
                                              'black) (sub1 n))]))]
    (draw-columns (draw-rows i NUMBER-OF-ROWS) NUMBER-OF-COLUMNS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on-key

;; World KeyEvent -> World
;; changes position and orientation of current Tetra
(define (shift-tetra w ke)
  (make-world (move/rotate-tetra (world-tetra w) (world-pile w) ke)
              (world-pile w)))

;; Tetra KeyEvent -> Tetra
;; moves or rotates the tetra
(define (move/rotate-tetra t blocks ke)
  (cond
    [(string=? ke "left") (move-tetra-left/right t 1 sub1 blocks)]
    [(string=? ke "right") (move-tetra-left/right t NUMBER-OF-COLUMNS add1 blocks)]
    [(string=? ke "s") (rotate-t (rotate-t (rotate-t t)))]
    [(string=? ke "a") (rotate-t t)]
    [(string=? ke "down") (move-tetra-down t blocks)]
    [else t]))

;; Tetra Number [Number -> Number] BSet -> Tetra
;; move the tetra to the left or right if permissible
(define (move-tetra-left/right t edge func blocks)
  (if (ormap (λ (a-block) (or (= (block-x a-block) edge)
                              (ormap (λ (a-pile-block) (and (= (func (block-x a-block))
                                                               (block-x a-pile-block))
                                                            (= (block-y a-block)
                                                               (block-y a-pile-block))))
                                     blocks)))
             (tetra-blocks t))
      t
      (make-tetra (update-center (tetra-center t) func)
                  (map (λ (a-block) (make-block (func (block-x a-block))
                                                (block-y a-block)
                                                (block-color a-block)))
                       (tetra-blocks t)))))

;; Posn [Number -> Number] -> Posn
;; updates the posn with the given function
(define (update-center p func)
  (make-posn (func (posn-x p))
             (posn-y p)))

;; Tetra -> Tetra
;; rotate the tetra counterclockwise 90 degrees
(define (rotate-t t)
  (make-tetra (tetra-center t)
              (map (λ (a-block) (block-rotate-ccw (tetra-center t) a-block))
                   (tetra-blocks t))))

;; Tetra -> Tetra
;; shifts tetra down 1 if possible
(define (move-tetra-down t blocks)
  (if (tetra-collide? t blocks)
      t
      (lower-tetra t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on-tick

;; World -> World
;; moves tetra down and deletes any necessary blocks
(define (update-world w)
  (maybe-remove/shift (maybe-update-pile/tetra (move-tetra w))))

;; World -> World
;; moves the tetra down one block if need be
(define (move-tetra w)
  (if (tetra-collide? (world-tetra w) (world-pile w))
      w
      (make-world (lower-tetra (world-tetra w)) (world-pile w))))

;; Tetra BSet -> Boolean
;; will the tetra collide with the bottom of the screen or other blocks?
(define (tetra-collide? t blocks)
  (ormap (λ (a-t-block)
           (or (= (block-y a-t-block) NUMBER-OF-ROWS)
               (ormap (λ (a-pile-block) (and (= (block-x a-t-block)
                                                (block-x a-pile-block))
                                             (= (add1 (block-y a-t-block))
                                                (block-y a-pile-block))))
                      blocks)))
         (tetra-blocks t)))

;; Tetra -> Tetra
;; lowers the tetra by 1
(define (lower-tetra t)
  (make-tetra (make-posn (posn-x (tetra-center t))
                         (add1 (posn-y (tetra-center t))))
              (map (λ (a-block) (make-block (block-x a-block)
                                            (add1 (block-y a-block))
                                            (block-color a-block)))
                   (tetra-blocks t))))

;; World -> World
;; possibly updates the pile and tetra if the tetra is now stuck
(define (maybe-update-pile/tetra w)
  (if (tetra-collide? (world-tetra w) (world-pile w))
      (make-world (generate-new-tetra (random 100) (+ 4 (random (- NUMBER-OF-COLUMNS 3))))
                  (update-pile (world-tetra w) (world-pile w)))
      w))

;; World -> World
;; removes a row of blocks and shifts everything else down one if necessary
(define (maybe-remove/shift w)
  (local [(define (check-rows n blocks)
            (cond
              [(zero? n) blocks]
              [else (if (= NUMBER-OF-COLUMNS
                           (foldr (λ (a-block result) (if (= n (block-y a-block))
                                                          (add1 result)
                                                          result))
                                  0
                                  blocks))
                        (check-rows (sub1 n) (shift-blocks (remove-blocks blocks n) n))
                        (check-rows (sub1 n) blocks))]))]
    (make-world (world-tetra w) (check-rows NUMBER-OF-ROWS (world-pile w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stop-when

;; World -> Boolean
;; is the tetra overlapping with any pile blocks?
(define (bricks-overlap? w)
  (ormap (λ (a-tetra-block)
           (ormap (λ (a-pile-block) (and (= (block-x a-tetra-block)
                                            (block-x a-pile-block))
                                         (= (block-y a-tetra-block)
                                            (block-y a-pile-block))))
                  (world-pile w)))
         (tetra-blocks (world-tetra w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

;; block-rotate-ccw : Posn Block -> Block
;; Rotate the block 90 counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (make-block (- (posn-x c)
                 (- (posn-y c) (block-y b)))
              (- (posn-y c)
                 (- (block-x b) (posn-x c)))
              (block-color b)))

;; Tetra BSet -> BSet
;; updates the BSet with all the blocks in the tetra
(define (update-pile t blocks)
  (foldr cons blocks (tetra-blocks t)))

;; BSet Number -> BSet
;; remove all blocks with the given number as a y position
(define (remove-blocks blocks n)
  (filter (λ (a-block) (not (= (block-y a-block) n))) blocks))

;; BSet Number -> BSet
;; shifts all blocks down one when a row is deleted
(define (shift-blocks blocks n)
  (map (λ (a-block) (if (< (block-y a-block) n)
                        (make-block (block-x a-block)
                                    (add1 (block-y a-block))
                                    (block-color a-block))
                        a-block)) blocks))

;; Number Number -> Number
;; translates grid coordinates into pixel coordinates
(define (update-coord grid-coord padding)
  (- (* padding grid-coord) (/ padding 2)))

;; Number Number -> Tetra
;; generates a new tetra for the world
(define (generate-new-tetra rand x-pos)
  (cond
    [(= (modulo rand 7) 0) (new-tetra-1 x-pos)]
    [(= (modulo rand 7) 1) (new-tetra-2 x-pos)]
    [(= (modulo rand 7) 2) (new-tetra-3 x-pos)]
    [(= (modulo rand 7) 3) (new-tetra-4 x-pos)]
    [(= (modulo rand 7) 4) (new-tetra-5 x-pos)]
    [(= (modulo rand 7) 5) (new-tetra-6 x-pos)]
    [(= (modulo rand 7) 6) (new-tetra-7 x-pos)]))

;; Number -> Tetra
;; creates a new tetra for the world
(define (new-tetra-1 n)
  (make-tetra (make-posn (- n .5) (- n .5))
              (list
               (make-block (sub1 n) 1 'green)
               (make-block (sub1 n) 2 'green)
               (make-block n 1 'green)
               (make-block n 2 'green))))

;; Number -> Tetra
;; creates a new tetra for the world
(define (new-tetra-2 n)
  (make-tetra (make-posn (sub1 n) 1)
              (list (make-block n 1 'blue)
                    (make-block (sub1 n) 1 'blue)
                    (make-block (- n 2) 1 'blue)
                    (make-block (- n 3) 1 'blue))))

;; Number -> Tetra
;; creates a new tetra for the world
(define (new-tetra-3 n)
  (make-tetra (make-posn n 2)
              (list (make-block n 1 'purple)
                    (make-block n 2 'purple)
                    (make-block (sub1 n) 2 'purple)
                    (make-block (- n 2) 2 'purple))))

;; Number -> Tetra
;; creates a new tetra for the world
(define (new-tetra-4 n)
  (make-tetra (make-posn (- n 2) 2)
              (list (make-block (- n 2) 1 'cyan)
                    (make-block (- n 2) 2 'cyan)
                    (make-block (sub1 n) 2 'cyan)
                    (make-block n 2 'cyan))))

;; Number -> Tetra
;; creates a new tetra for the world
(define (new-tetra-5 n)
  (make-tetra (make-posn (sub1 n) 2)
              (list (make-block n 2 'orange)
                    (make-block (sub1 n) 2 'orange)
                    (make-block (sub1 n) 1 'orange)
                    (make-block (- n 2) 2 'orange))))

;; Number -> Tetra
;; creates a new tetra for the world
(define (new-tetra-6 n)
  (make-tetra (make-posn (sub1 n) 1)
              (list (make-block n 2 'pink)
                    (make-block (sub1 n) 2 'pink)
                    (make-block (sub1 n) 1 'pink)
                    (make-block (- n 2) 1 'pink))))

;; Number -> Tetra
;; creates a new tetra for the world
(define (new-tetra-7 n)
  (make-tetra (make-posn (sub1 n) 1)
              (list (make-block n 1 'red)
                    (make-block (sub1 n) 1 'red)
                    (make-block (sub1 n) 2 'red)
                    (make-block (- n 2) 2 'red))))