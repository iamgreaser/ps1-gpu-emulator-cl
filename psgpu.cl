;; vim: set sts=2 sw=2 et sm lisp :

(declaim (optimize
           (speed 3)
           (safety 1)
           (debug 0)
           (space 0)))

(defvar *screen-window* nil)
(defvar *screen-renderer* nil)
(defvar *screen-texture* nil)

(defparameter *gp0-opcodes*
  (make-array 256 :initial-element nil))

(ql:quickload :sdl2)

(defmacro swap-pair (p1 p2)
  `(multiple-value-setq (,p1 ,p2) (values ,p2 ,p1)))

(defparameter *bilerpers* t)

(defmacro lets-for-triangle (&body forms)
  (labels ((let-for-triangle (dst-prefix src-prefix base-form)
             (mapcar
               #'(lambda (index)
                   (list
                     (intern (format nil "~a~d"
                                     (symbol-name dst-prefix)
                                     index))
                     (sublis (list
                               (cons
                                 '$x
                                 (if src-prefix
                                   (intern (format nil "~a~d"
                                                   (symbol-name src-prefix)
                                                   index))
                                   index)))
                              base-form)))
               '(0 1 2))))
    `',(mapcan #'(lambda (x)
                   (apply #'let-for-triangle x))
               forms)))

(defmacro lets-for-lerp-steps-1 (&body forms)
  (labels ((/transform (sym-base v0 v1 v2)
             (sublis
               `(($-top . ,(intern (format nil "~a-TOP" sym-base)))
                 ($-mid . ,(intern (format nil "~a-MID" sym-base)))
                 ($-bot . ,(intern (format nil "~a-BOT" sym-base)))
                 ($-top-step . ,(intern (format nil "~a-TOP-STEP" sym-base)))
                 ($-bot-step . ,(intern (format nil "~a-BOT-STEP" sym-base)))
                 ($-maj-step . ,(intern (format nil "~a-MAJ-STEP" sym-base)))
                 )

               `(($-top ,v0)
                 ($-mid ,v1)
                 ($-bot ,v2)

                 ($-top-step (floor (/ (* (- $-mid $-top) #x1000)
                                     (max 1 (- y-mid y-top)))))
                 ($-bot-step (floor (/ (* (- $-bot $-mid) #x1000)
                                     (max 1 (- y-bot y-mid)))))
                 ($-maj-step (floor (/ (* (- $-bot $-top) #x1000)
                                     (max 1 (- y-bot y-top)))))
                 ))))
    `',(mapcan #'(lambda (x)
                   (apply #'/transform x))
               forms)))


(defclass psgpu ()
  ((vram
     :type (simple-array (unsigned-byte 16) (524288))
     :initform (make-array (* 1024 512)
                           :element-type '(unsigned-byte 16)
                           :initial-element 0))
   (gp0-buffer
     :type (simple-array (unsigned-byte 32) (16))
     :initform (make-array 16
                           :element-type '(unsigned-byte 32)
                           :initial-element 0))
   (clamp-x1 :type fixnum :initform 0)
   (clamp-y1 :type fixnum :initform 0)
   (clamp-x2 :type fixnum :initform 1023)
   (clamp-y2 :type fixnum :initform 511)
   (offs-x :type fixnum :initform 0)
   (offs-y :type fixnum :initform 0)
   (disp-xbeg :type fixnum :initform 0)
   (disp-ybeg :type fixnum :initform 0)
   (screen-x1 :type fixnum :initform #x200)
   (screen-x2 :type fixnum :initform #xC00)
   (screen-y1 :type fixnum :initform #x010)
   (screen-y2 :type fixnum :initform #x100)
   (screen-pixel-width :type fixnum :initform 8)
   (screen-interlace-multiplier :type fixnum :initform 1)
   (global-texpage :type fixnum :initform 0)
   (gp0-buffer-length :type fixnum
                      :initform 0)

   (transfer-halfwords-in :type fixnum
                          :initform 0)
   (transfer-width        :type fixnum
                          :initform 0)
   (transfer-height       :type fixnum
                          :initform 0)
   (transfer-reset-x      :type fixnum
                          :initform 0)
   (transfer-remaining-x  :type fixnum
                          :initform 0)
   (transfer-dx           :type fixnum
                          :initform 0)
   (transfer-dy           :type fixnum
                          :initform 0)
   ))

(defun make-psgpu ()
  (let* ((this (make-instance 'psgpu)))
    (with-slots () this
      nil)
    this))

(defgeneric write-gp0 (this data))
(defgeneric write-gp1 (this data))

;; assuming new-gpu here (old-gpu ignores lower 3 bits when modulating)
(defun color-24-to-15 (bgr)
  (declare (inline))
  (declare (type fixnum bgr))
  (let* ((lb (logand #xFF (ash bgr -0)))
         (lg (logand #xFF (ash bgr -8)))
         (lr (logand #xFF (ash bgr -16)))
         (sb (ash lb -3))
         (sg (ash lg -3))
         (sr (ash lr -3)))
    (logior
      (ash sb 0)
      (ash sg 5)
      (ash sr 10))))

(defun color-15-to-24 (bgr)
  (declare (inline))
  (declare (type fixnum bgr))
  (let* ((sb (logand #x1F (ash bgr -0)))
         (sg (logand #x1F (ash bgr -5)))
         (sr (logand #x1F (ash bgr -10)))
         (lb (ash sb 3))
         (lg (ash sg 3))
         (lr (ash sr 3)))
    (logior
      (ash lb 0)
      (ash lg 8)
      (ash lr 16))))

(defun convert-clut-pointer (clut)
  (declare (type fixnum clut))
  (+ (* 16   (logand #x003F (ash clut -0)))
     (* 1024 (logand #x01FF (ash clut -6)))))

(defun convert-texpage-pointer (texpage)
  (declare (type fixnum texpage))
  (+ (* 64       (logand #x000F (ash texpage -0)))
     (* 1024 256 (logand #x0001 (ash texpage -4)))))

;(defmethod putpixel-unclamped ((this psgpu) x y color15)
(defun putpixel-unclamped (this x y color15)
  (declare (inline))
  (declare (type psgpu this))
  (declare (type fixnum x y color15))
  (with-slots (vram) this
    (when (and (>= x 0)
               (>= y 0)
               (<  x 1024)
               (<  y 512))
      (setf (aref vram (+ x (ash y 10)))
            color15))))

(defun putpixel (this x y color15)
  (declare (inline))
  (declare (type psgpu this))
  (declare (type fixnum x y color15))
  (with-slots (clamp-x1 clamp-y1
               clamp-x2 clamp-y2
               offs-x offs-y) this
    (let* ((x (+ x offs-x))
           (y (+ y offs-y)))
      (when (and (>= x clamp-x1)
                 (>= y clamp-y1)
                 (<= x clamp-x2)
                 (<= y clamp-y2))
        (putpixel-unclamped this x y color15)))))


(defun build-gp0-case (index)
  (labels ((/build-geom-body (index)
             (assert (>= index #x20))
             (assert (<= index #x7F))
             (ecase (logand index #xE0)
               ((#x20) (/draw-poly index))
               ((#x40) (/draw-line index))
               ((#x60) (/draw-rect index))))

           (/draw-poly (index)
             (let* ((*bilerpers*      (list))
                    (raw-textured     (= #x05 (logand index #x05)))
                    (semi-transparent (/= 0 (logand index #x02)))
                    (texture-mapped   (/= 0 (logand index #x04)))
                    (is-quad          (/= 0 (logand index #x08)))
                    (gouraud-shaded   (/= 0 (logand index #x10)))
                    (points           3)
                    (words-for-color  (if gouraud-shaded points 1))
                    (args-per-point   (if texture-mapped 2 1))
                    (words-needed     (+ (* points args-per-point)
                                         words-for-color))
                    (color-offset     0)
                    (vertex-offset    1)
                    (texcoord-offset  2)
                    (color-step       (if gouraud-shaded
                                        (+ args-per-point 1)
                                        0))
                    (vertex-step      (+ args-per-point
                                         (if gouraud-shaded 1 0)))
                    (texcoord-step    (+ args-per-point
                                         (if gouraud-shaded 1 0)))
                    )
               (declare (ignore semi-transparent raw-textured))
               (labels
                 ((/draw-poly-half-flat (upper-y lower-y)
                    `((dotimes (yi (- ,lower-y ,upper-y))
                        (let* ((xlpixel (max 0 clamp-x1 (ash xlpos -12)))
                               (xrpixel (min 1024 clamp-x2 (ash xrpos -12)))
                               (ypixel (+ ,upper-y yi)))
                          (declare (type fixnum xlpixel xrpixel ypixel))
                          (assert (and (<= 0 ypixel 511)
                                       (<= clamp-y1 ypixel clamp-y2)))
                          (let* ((ibase (+ xlpixel (* ypixel 1024))))
                            (declare (type fixnum ibase))
                            (dotimes (xi (- xrpixel xlpixel))
                              (setf (aref vram (+ ibase xi)) cd0p))))
                        (incf xlpos xlstep)
                        (incf xrpos xrstep))))

                  (/draw-poly-half-rawtex (upper-y lower-y)
                    `((dotimes (yi (- ,lower-y ,upper-y))
                        (let* ((xlpixel (max 0 clamp-x1 (ash xlpos -12)))
                               (xrpixel (min 1024 clamp-x2 (ash xrpos -12)))
                               (sp slpos)
                               (tp tlpos)
                               (spspan (- srpos slpos))
                               (tpspan (- trpos tlpos))
                               (splocstep (floor (/ spspan
                                                    (max 1 (- xrpixel xlpixel)))))
                               (tplocstep (floor (/ tpspan
                                                    (max 1 (- xrpixel xlpixel)))))
                               (ypixel (+ ,upper-y yi)))
                          (declare (type fixnum xlpixel xrpixel ypixel))
                          (assert (and (<= 0 ypixel 511)
                                       (<= clamp-y1 ypixel clamp-y2)))
                          (let* ((ibase (+ xlpixel (* ypixel 1024))))
                            (declare (type fixnum ibase))
                            (dotimes (xi (- xrpixel xlpixel))
                              (let* ((tx (ash sp -12))
                                     (ty (ash tp -12))
                                     (pixelpos
                                       (ecase texbpp
                                         ((4)
                                            (+ clutref
                                               (logand #x000F
                                                 (ash (aref vram
                                                        ;; FIXME: range is busted
                                                        (logand
                                                          #x7FFFF
                                                          (+ texref
                                                             (ash tx -2)
                                                             (* ty 1024))))
                                                      (* -4 (logand #x3 tx))))))
                                         ((8)
                                            (+ clutref
                                               (logand #x00FF
                                                 (ash (aref vram
                                                        (logand
                                                          #x7FFFF
                                                          (+ texref
                                                             (ash tx -1)
                                                             (* ty 1024))))
                                                      (* -8 (logand #x1 tx))))))
                                         ((15) (+ tx (* ty 1024)))))
                                     (pixeldata (aref vram
                                                      (logand #x7FFFF pixelpos))))
                                (when (/= 0 pixeldata)
                                  (setf (aref vram (+ ibase xi)) pixeldata))
                                (incf sp splocstep)
                                (incf tp tplocstep))))
                        (incf xlpos xlstep)
                        (incf xrpos xrstep)
                        (incf slpos slstep)
                        (incf srpos srstep)
                        (incf tlpos tlstep)
                        (incf trpos trstep)
                        ))))

                  (/draw-poly-half-gouraud (upper-y lower-y)
                    `((dotimes (yi (- ,lower-y ,upper-y))
                        (let* ((xlpixel (max 0 clamp-x1 (ash xlpos -12)))
                               (xrpixel (min 1023 clamp-x2 (ash xrpos -12)))
                               (ypixel (+ ,upper-y yi))
                               ;; TODO: get a constant horizontal delta
                               (c-r clpos-r)
                               (c-g clpos-g)
                               (c-b clpos-b)
                               (cspan-r (- crpos-r clpos-r))
                               (cspan-g (- crpos-g clpos-g))
                               (cspan-b (- crpos-b clpos-b))
                               (clocstep-r (floor (/ cspan-r
                                                     (max 1 (- xrpixel xlpixel)))))
                               (clocstep-g (floor (/ cspan-g
                                                     (max 1 (- xrpixel xlpixel)))))
                               (clocstep-b (floor (/ cspan-b
                                                     (max 1 (- xrpixel xlpixel)))))
                               )
                          (declare (type fixnum xlpixel xrpixel ypixel))
                          (declare (type fixnum c-r c-g c-b))
                          (declare (type fixnum cspan-r cspan-g cspan-b))
                          (declare (type fixnum clocstep-r clocstep-g clocstep-b))
                          (assert (and (<= 0 ypixel 511)
                                       (<= clamp-y1 ypixel clamp-y2)))
                          (let* ((ibase (+ xlpixel (* ypixel 1024)))
                                 )
                            (declare (type fixnum ibase))
                            (dotimes (xi (- xrpixel xlpixel))
                              ;(let* ((cd0p (logior (ash (logand #xF8
                              ;                                  (max 0 (min 255
                              ;                                              (ash c-r -12)))) -3)
                              ;                     (ash (logand #xF8
                              ;                                  (max 0 (min 255
                              ;                                              (ash c-g -12))))  2)
                              ;                     (ash (logand #xF8
                              ;                                  (max 0 (min 255
                              ;                                              (ash c-b -12))))  7))))
                              ;

                              ;; may potentially artifact
                              ;(let* ((cd0p (logior (ash (logand #xF8 (ash c-r -12)) -3)
                              ;                     (ash (logand #xF8 (ash c-g -12))  2)
                              ;                     (ash (logand #xF8 (ash c-b -12))  7))))
                              (let* ((cd0p (logior (ash (logand #xF8 (ash (+ c-r (* xi clocstep-r))
                                                                          -12)) -3)
                                                   (ash (logand #xF8 (ash (+ c-g (* xi clocstep-g))
                                                                          -12))  2)
                                                   (ash (logand #xF8 (ash (+ c-b (* xi clocstep-b))
                                                                          -12))  7))))
                                ;
                                (declare (type fixnum cd0p))
                                ;(incf c-r clocstep-r)
                                ;(incf c-g clocstep-g)
                                ;(incf c-b clocstep-b)
                                (setf (aref vram (+ ibase xi)) cd0p)))))
                        (incf xlpos xlstep)
                        (incf xrpos xrstep)
                        (incf clpos-r clstep-r)
                        (incf crpos-r crstep-r)
                        (incf clpos-g clstep-g)
                        (incf crpos-g crstep-g)
                        (incf clpos-b clstep-b)
                        (incf crpos-b crstep-b)
                        )))

                  (/draw-poly-half (upper-y lower-y)
                    (cond
                      (texture-mapped
                        ;; TODO!
                        (/draw-poly-half-rawtex  upper-y lower-y))
                      (gouraud-shaded
                        (/draw-poly-half-gouraud upper-y lower-y))
                      (t
                        (/draw-poly-half-flat    upper-y lower-y))))
                  )

                 ;;

                 `((when (< gp0-buffer-length ,words-needed)
                     (return-from keep-gp0-buffer nil))
                   (let* ((vd0 (aref gp0-buffer ,(+ vertex-offset (* vertex-step 0))))
                          (vd1 (aref gp0-buffer ,(+ vertex-offset (* vertex-step 1))))
                          (vd2 (aref gp0-buffer ,(+ vertex-offset (* vertex-step 2))))
                          ,@(lets-for-triangle
                              (x vd (- (logand (+ $x #x0400) #x07FF) #x0400))
                              (y vd (- (logand (+ (ash $x -16) #x0400) #x07FF) #x0400)))
                          (cd0 (aref gp0-buffer ,(+ color-offset  (* color-step  0))))
                          ,@(unless (or texture-mapped gouraud-shaded)
                            `(
                              (cd0p (color-24-to-15 cd0))))
                          ,@(when gouraud-shaded
                              `(
                                (cd1 (aref gp0-buffer ,(+ color-offset  (* color-step  1))))
                                (cd2 (aref gp0-buffer ,(+ color-offset  (* color-step  2))))
                                ,@(lets-for-triangle
                                    (cr cd (logand #xFF (ash $x  -0)))
                                    (cg cd (logand #xFF (ash $x  -8)))
                                    (cb cd (logand #xFF (ash $x -16)))
                                    )
                                ))
                          ,@(when texture-mapped
                              `((td0 (aref gp0-buffer ,(+ texcoord-offset (* texcoord-step 0))))
                                (td1 (aref gp0-buffer ,(+ texcoord-offset (* texcoord-step 1))))
                                (td2 (aref gp0-buffer ,(+ texcoord-offset (* texcoord-step 2))))
                                ,@(lets-for-triangle
                                    (s td (logand $x #x00FF))
                                    (t td (logand (ash $x -8) #x00FF)))
                                ))
                          ,@(when texture-mapped
                              `((texpage (ash td1 -16))
                                (texbpp
                                  (ecase (logand #x3 (ash texpage -7))
                                    ((0)  4)
                                    ((1)  8)
                                    ((2 3) 15)))  ; FIXME this seems to be wrong, why do I get this?
                                (clutref
                                  (convert-clut-pointer
                                    (ash td0 -16)))
                                (texref
                                  (convert-texpage-pointer
                                    texpage))))
                          )
                     (declare (ignorable cd0))
                     (declare (type fixnum x0 y0 x1 y1 x2 y2))

                     ;; TODO: cancel oversized polys

                     (with-slots (clamp-x1 clamp-y1
                                  clamp-x2 clamp-y2
                                  offs-x offs-y) this
                       (incf x0 offs-x)
                       (incf x1 offs-x)
                       (incf x2 offs-x)
                       (incf y0 offs-y)
                       (incf y1 offs-y)
                       (incf y2 offs-y)

                       ;; TODO: actually clip instead of min/max
                       (setf x0 (max clamp-x1 (min clamp-x2 x0)))
                       (setf x1 (max clamp-x1 (min clamp-x2 x1)))
                       (setf x2 (max clamp-x1 (min clamp-x2 x2)))
                       (setf y0 (max clamp-y1 (min clamp-y2 y0)))
                       (setf y1 (max clamp-y1 (min clamp-y2 y1)))
                       (setf y2 (max clamp-y1 (min clamp-y2 y2)))

                       ;; Order such that (<= y0 y1 y2)
                       ;; Stoogesort is definitely worthwhile here
                       (when (< y2 y1)
                         (swap-pair x1 x2)
                         ,@(when gouraud-shaded
                             `((swap-pair cr1 cr2)
                               (swap-pair cg1 cg2)
                               (swap-pair cb1 cb2)
                               ))
                         ,@(when texture-mapped
                             `((swap-pair s1 s2)
                               (swap-pair t1 t2)))
                         (swap-pair y1 y2)
                         )
                       (when (< y1 y0)
                         (swap-pair x0 x1)
                         ,@(when gouraud-shaded
                             `((swap-pair cr0 cr1)
                               (swap-pair cg0 cg1)
                               (swap-pair cb0 cb1)
                               ))
                         ,@(when texture-mapped
                             `((swap-pair s0 s1)
                               (swap-pair t0 t1)))
                         (swap-pair y0 y1)
                         )
                       (when (< y2 y1)
                         (swap-pair x1 x2)
                         ,@(when gouraud-shaded
                             `((swap-pair cr1 cr2)
                               (swap-pair cg1 cg2)
                               (swap-pair cb1 cb2)
                               ))
                         ,@(when texture-mapped
                             `((swap-pair s1 s2)
                               (swap-pair t1 t2)))
                         (swap-pair y1 y2)
                         )

                       (assert (<= y0 y1 y2))

                       (let* ((y-top y0)
                              (y-bot y2)
                              (y-mid y1)
                              ,@(lets-for-lerp-steps-1
                                  (x x0 x1 x2))

                              ,@(when gouraud-shaded
                                  `(,@(lets-for-lerp-steps-1
                                        (cr cr0 cr1 cr2)
                                        (cg cg0 cg1 cg2)
                                        (cb cb0 cb1 cb2)
                                        )
                                    ))

                              ,@(when texture-mapped
                                  `(,@(lets-for-lerp-steps-1
                                        (s s0 s1 s2)
                                        (t t0 t1 t2)
                                        )
                                    ))

                              (left-major
                                (if (= y-top y-mid)
                                  (> x-maj-step x-bot-step)
                                  (< x-maj-step x-top-step)))
                              (xlpos  (+ (ash x-top 12) #x0800))
                              (xrpos  (+ (ash x-top 12) #x0800))
                              (xlstep (if left-major x-maj-step x-top-step))
                              (xrstep (if left-major x-top-step x-maj-step))
                              ,@(when gouraud-shaded
                                  `((clpos-r  (+ (ash cr-top 12) #x0800))
                                    (crpos-r  (+ (ash cr-top 12) #x0800))
                                    (clstep-r (if left-major cr-maj-step cr-top-step))
                                    (crstep-r (if left-major cr-top-step cr-maj-step))
                                    (clpos-g  (+ (ash cg-top 12) #x0800))
                                    (crpos-g  (+ (ash cg-top 12) #x0800))
                                    (clstep-g (if left-major cg-maj-step cg-top-step))
                                    (crstep-g (if left-major cg-top-step cg-maj-step))
                                    (clpos-b  (+ (ash cb-top 12) #x0800))
                                    (crpos-b  (+ (ash cb-top 12) #x0800))
                                    (clstep-b (if left-major cb-maj-step cb-top-step))
                                    (crstep-b (if left-major cb-top-step cb-maj-step))
                                    ))
                              ,@(when texture-mapped
                                  `((slpos  (+ (ash s-top 12) #x0800))
                                    (srpos  (+ (ash s-top 12) #x0800))
                                    (slstep (if left-major s-maj-step s-top-step))
                                    (srstep (if left-major s-top-step s-maj-step))
                                    (tlpos  (+ (ash t-top 12) #x0800))
                                    (trpos  (+ (ash t-top 12) #x0800))
                                    (tlstep (if left-major t-maj-step t-top-step))
                                    (trstep (if left-major t-top-step t-maj-step))
                                    ))
                              )
                         (declare (type fixnum y-top y-mid y-bot x-top x-mid x-bot))
                         (declare (type fixnum x-top-step x-bot-step x-maj-step))
                         (declare (type fixnum xlpos xrpos xlstep xrstep))
                         ,@(when texture-mapped
                             `((declare (type fixnum slpos srpos slstep srstep))
                               (declare (type fixnum tlpos trpos tlstep trstep))
                               ))

                         ;; Do top
                         ,@(/draw-poly-half 'y-top 'y-mid)

                         ;; Snap minor
                         (if left-major
                           (progn
                             (setf xrpos  (+ (ash x-mid 12) #x0800))
                             (setf xrstep x-bot-step)
                             ,@(when gouraud-shaded
                                 `(;
                                   (setf crpos-r  (+ (ash cr-mid 12) #x0800))
                                   (setf crstep-r cr-bot-step)
                                   (setf crpos-g  (+ (ash cg-mid 12) #x0800))
                                   (setf crstep-g cg-bot-step)
                                   (setf crpos-b  (+ (ash cb-mid 12) #x0800))
                                   (setf crstep-b cb-bot-step)
                                   ))
                             ,@(when texture-mapped
                                 `(;
                                   (setf srpos  (+ (ash s-mid 12) #x0800))
                                   (setf srstep s-bot-step)
                                   (setf trpos  (+ (ash t-mid 12) #x0800))
                                   (setf trstep t-bot-step)
                                   ))
                             )
                           (progn
                             (setf xlpos  (+ (ash x-mid 12) #x0800))
                             (setf xlstep x-bot-step)
                             ,@(when gouraud-shaded
                                 `(;
                                   (setf clpos-r  (+ (ash cr-mid 12) #x0800))
                                   (setf clstep-r cr-bot-step)
                                   (setf clpos-g  (+ (ash cg-mid 12) #x0800))
                                   (setf clstep-g cg-bot-step)
                                   (setf clpos-b  (+ (ash cb-mid 12) #x0800))
                                   (setf clstep-b cb-bot-step)
                                   ))
                             ,@(when texture-mapped
                                 `(;
                                   (setf slpos  (+ (ash s-mid 12) #x0800))
                                   (setf slstep s-bot-step)
                                   (setf tlpos  (+ (ash t-mid 12) #x0800))
                                   (setf tlstep t-bot-step)
                                   ))
                             ))

                         ;; Do bottom
                         ,@(/draw-poly-half 'y-mid 'y-bot)

                         )))

                   ;; Convert quads to tris

                   ,@(when (and is-quad texture-mapped)
                       `(;; Copy CLUT, Texpage upwards
                         (setf (aref gp0-buffer ,(+ (* texcoord-step 2)
                                                    texcoord-offset))
                               (logior (logand (aref gp0-buffer ,(+ (* texcoord-step 1)
                                                                    texcoord-offset))
                                                     #xFFFF0000)
                                       (logand (aref gp0-buffer ,(+ (* texcoord-step 2)
                                                                    texcoord-offset))
                                                     #x0000FFFF)))

                         (setf (aref gp0-buffer ,(+ (* texcoord-step 1)
                                                    texcoord-offset))
                               (logior (logand (aref gp0-buffer ,(+ (* texcoord-step 0)
                                                                    texcoord-offset))
                                                     #xFFFF0000)
                                       (logand (aref gp0-buffer ,(+ (* texcoord-step 1)
                                                                    texcoord-offset))
                                                     #x0000FFFF)))
                         ))

                   ,@(when is-quad
                       (if gouraud-shaded
                         ;; Gouraud version
                         `(;; Copy and set command
                           (setf (aref gp0-buffer ,color-step)
                                 (logior (logand (aref gp0-buffer 0) #xF7000000)
                                         (logand (aref gp0-buffer ,color-step) #x00FFFFFF)))

                           ;; Overwrite
                           (dotimes (i (- gp0-buffer-length ,color-step))
                             (setf (aref gp0-buffer i)
                                   (aref gp0-buffer (+ i ,color-step))))

                           ;; Return
                           (decf gp0-buffer-length ,color-step)
                           (return-from keep-gp0-buffer nil))

                         ;; Flat version
                         `(;; Set command
                           (setf (aref gp0-buffer 0)
                                 (logand (aref gp0-buffer 0) #xF7FFFFFF))

                           ;; Overwrite
                           (dotimes (i (- gp0-buffer-length ,vertex-step 1))
                             (setf (aref gp0-buffer (+ 1 i))
                                   (aref gp0-buffer (+ 1 i ,vertex-step))))

                           ;; Return
                           (decf gp0-buffer-length ,vertex-step)
                           (return-from keep-gp0-buffer nil))))
                   ))))

           (/draw-line (index)
             (let* ((semi-transparent (/= 0 (logand index #x02)))
                    (poly-line        (/= 0 (logand index #x08)))
                    (gouraud-shaded   (/= 0 (logand index #x10)))
                    (words-for-color  (if gouraud-shaded 2 1))
                    (words-needed     (+ 2 words-for-color))
                    (color-offset     0)
                    (vertex-offset    1)
                    (color-step       (if gouraud-shaded 2 0))
                    (vertex-step      (if gouraud-shaded 2 1))
                    )
               (declare (ignore semi-transparent))
               `(;; TODO: handle poly-lines sanely
                 ,@(when poly-line
                   `((error (format nil "TODO: ~2,'8X polyline" ,index))))
                 (when (< gp0-buffer-length ,words-needed)
                   (return-from keep-gp0-buffer nil))
                 ;(format t "line ~2,'8X wcount=~d~%" ,index ,words-needed)
                 (let* ((vi0 ,(+ vertex-offset (* vertex-step 0)))
                        (vi1 ,(+ vertex-offset (* vertex-step 1)))
                        (vd0 (aref gp0-buffer vi0))
                        (vd1 (aref gp0-buffer vi1))
                        (ci0 ,(+ color-offset  (* color-step  0)))
                        (cd0 (aref gp0-buffer ci0))
                        (cd0p (color-24-to-15 cd0))
                        ,@(if (and nil gouraud-shaded)
                            `((ci1 ,(+ color-offset  (* color-step  1)))
                              (cd1 (aref gp0-buffer ci1))
                              (cd1p (color-24-to-15 cd1))))
                        (x0  (- (logand (+ vd0 #x0400) #x07FF) #x0400))
                        (y0  (- (logand (+ (ash vd0 -16) #x0400) #x07FF) #x0400))
                        (x1  (- (logand (+ vd1 #x0400) #x07FF) #x0400))
                        (y1  (- (logand (+ (ash vd1 -16) #x0400) #x07FF) #x0400))
                        (dx (- x1 x0))
                        (dy (- y1 y0))
                        )

                   ;; TODO: gouraud shading
                   ;; TODO: find a reasonably accurate algorithm for xctr/yctr
                   (if (> (abs dx) (abs dy))
                     ;; horizontal-major
                     (let* ((xbeg  (if (< x0 x1) x0 x1))
                            (xend  (if (< x0 x1) x1 x0))
                            (ybeg  (if (< x0 x1) y0 y1))
                            (yend  (if (< x0 x1) y1 y0))
                            (xlen  (- xend xbeg))
                            (ylen  (- yend ybeg))
                            (xlena (abs xlen))
                            (ylena (abs ylen))
                            (ystep (if (< yend ybeg) -1 1))
                            (yctr  0)
                            (y-mid  ybeg))
                       (declare (type fixnum xbeg xend ybeg yend xlen ylen xlena ylena ystep yctr y-mid))
                       (dotimes (xi (+ xlen 1))
                         (putpixel this (+ xbeg xi) y-mid cd0p)
                         (decf yctr ylena)
                         (when (< yctr 0)
                           (incf yctr xlena)
                           (incf y-mid ystep)))))

                     ;; vertical-major
                     (let* ((xbeg  (if (< y0 y1) x0 x1))
                            (xend  (if (< y0 y1) x1 x0))
                            (ybeg  (if (< y0 y1) y0 y1))
                            (yend  (if (< y0 y1) y1 y0))
                            (xlen  (- xend xbeg))
                            (ylen  (- yend ybeg))
                            (xlena (abs xlen))
                            (ylena (abs ylen))
                            (xstep (if (< xend xbeg) -1 1))
                            (xctr  0)
                            (x-mid  xbeg))
                       (declare (type fixnum ybeg yend xbeg xend ylen xlen ylena xlena xstep xctr x-mid))
                       (dotimes (yi (+ ylen 1))
                         (putpixel this x-mid (+ ybeg yi) cd0p)
                         (decf xctr xlena)
                         (when (< xctr 0)
                           (incf xctr ylena)
                           (incf x-mid xstep)))))
                 )))

           (/draw-rect (index)
             (let* ((raw-textured     (= #x05 (logand index #x05)))
                    (semi-transparent (/= 0 (logand index #x02)))
                    (texture-mapped   (/= 0 (logand index #x04)))
                    (rect-size-enum   (logand #x03 (ash index -3)))
                    (variably-sized   (= rect-size-enum 0))
                    (words-needed     (+ 2
                                         (if texture-mapped 1 0)
                                         (if variably-sized 1 0)))
                    (color-offset     0)
                    (vertex-offset    1)
                    (texcoord-offset  2)
                    (size-offset      (if texture-mapped 3 2))
                    )
               (declare (ignore semi-transparent raw-textured))
               `((when (< gp0-buffer-length ,words-needed)
                   (return-from keep-gp0-buffer nil))
                 ;(format t "rect ~2,'8X wcount=~d~%" ,index ,words-needed)
                 (with-slots (global-texpage) this
                   (let* ((cd  (aref gp0-buffer ,color-offset))
                          (vd  (aref gp0-buffer ,vertex-offset))
                          (bx  (- (logand (+ vd #x0400) #x07FF) #x0400))
                          (by  (- (logand (+ (ash vd -16) #x0400) #x07FF) #x0400))
                          ,@(when texture-mapped
                              `((texbpp
                                  (ecase (logand #x3
                                                 (ash global-texpage -7))
                                    ((0)  4)
                                    ((1)  8)
                                    ((2) 15)))
                                (td  (aref gp0-buffer ,texcoord-offset))
                                (clutref
                                  (convert-clut-pointer
                                    (ash td -16)))
                                (texref
                                  (convert-texpage-pointer
                                    global-texpage))
                                (btx (logand td #xFF))
                                (bty (logand (ash td -8) #xFF))
                                ))
                          ,@(when variably-sized
                              `((sd (aref gp0-buffer ,size-offset))))
                          (width ,(ecase rect-size-enum
                                    ((0) `(logand #x3FF sd))
                                    ((1) 1)
                                    ((2) 8)
                                    ((3) 16)))
                          (height ,(ecase rect-size-enum
                                    ((0) `(logand #x1FF (ash sd -16)))
                                    ((1) 1)
                                    ((2) 8)
                                    ((3) 16)))
                          )
                     (declare (type fixnum bx by width height))
                     (declare (ignorable cd))
                     (dotimes (yi height)
                       (dotimes (xi width)
                         (let* ((x (+ xi bx))
                                (y (+ yi by))
                                ,@(when texture-mapped
                                    `((tx (+ xi btx))
                                      (ty (+ yi bty))))
                                )
                           (declare (type fixnum x y
                                          ,@(when texture-mapped `(tx ty))
                                          ))
                           ,@(cond
                               ;; TODO: non-raw textures
                               ;; TODO: hoist this out depending on bpp mode
                               (texture-mapped
                                 `((let* ((pixelpos
                                            (ecase texbpp
                                              ((4)
                                                 (+ clutref
                                                    (logand #x000F
                                                      (ash (aref vram
                                                             (+ texref
                                                                (ash tx -2)
                                                                (* ty 1024)))
                                                           (* -4 (logand #x3 tx))))))
                                              ((8)
                                                 (+ clutref
                                                    (logand #x00FF
                                                      (ash (aref vram
                                                             (+ texref
                                                                (ash tx -1)
                                                                (* ty 1024)))
                                                           (* -8 (logand #x1 tx))))))
                                              ((15) (+ tx (* ty 1024)))))
                                          (pixeldata (aref vram pixelpos)))
                                     (when (/= 0 pixeldata)
                                       (putpixel this x y pixeldata)))))
                               (t
                                 `((putpixel this x y (color-24-to-15 cd))))
                               )
                           )))
                     ))
                   )))
           )
    (/build-geom-body index)))

(defmacro ecase-with-gp0-geometry (test &body maintests)
  (labels ((/loop-geom (index)
             (when (<= index #x7F)
               (assert (>= index #x20))
               (cons
                 ;`((,index) ,@(build-gp0-case index))
                 `((,index)
                   (unless (funcall ,(aref *gp0-opcodes* index) this)
                     (return-from keep-gp0-buffer nil)))
                 (/loop-geom (1+ index))))))
    `(ecase ,test
       ,@(/loop-geom #x20)
       ,@maintests)))

(defun build-gp0-case-body (index)
  `(with-slots (gp0-buffer
                gp0-buffer-length
                vram) this
     (declare (type (simple-array (unsigned-byte 16) (524288)) vram))
     (declare (type (simple-array (unsigned-byte 32) (16)) gp0-buffer))
     (declare (type fixnum gp0-buffer-length))
     ,@(build-gp0-case index)))

(do ((i #x20 (1+ i)))
  ((> i #x7F))
  (setf (aref *gp0-opcodes* i)
        (eval
          `(lambda (this)
             (block keep-gp0-buffer
               ,(build-gp0-case-body i)
               t)))))

(defmethod write-gp0 ((this psgpu) data)
  (declare (type (unsigned-byte 32) data))
  (declare (inline))
  (with-slots (gp0-buffer
               gp0-buffer-length
               vram) this
    (declare (type (simple-array (unsigned-byte 16) (524288)) vram))
    (declare (type (simple-array (unsigned-byte 32) (16)) gp0-buffer))
    (declare (type fixnum gp0-buffer-length))

    (assert (< gp0-buffer-length 16))
    (setf (aref gp0-buffer gp0-buffer-length) data)
    (incf gp0-buffer-length)
    (assert (<= gp0-buffer-length 16))
    ;(format t "GP0: ~8,'0X~%" data)
    (let* ((command-word (aref gp0-buffer 0)))
      (declare (type fixnum command-word))
      (block keep-gp0-buffer
        (ecase-with-gp0-geometry (logand (ash command-word -24) #xFF)
          ((#x00) nil)   ; NOP
          ((#x01) nil)   ; TODO: Clear cache

          ((#x02)  ; TODO: Fill Rectangle
           (when (< gp0-buffer-length 3)
             (return-from keep-gp0-buffer nil))
           (let* ((width   (logand #xFFFF (aref gp0-buffer 2)))
                  (height  (ash (aref gp0-buffer 2) -16))
                  (base-x  (logand #xFFFF (aref gp0-buffer 1)))
                  (base-y  (ash (aref gp0-buffer 1) -16))
                  (color24 command-word)
                  (color15 (color-24-to-15 color24))
                  )
             (declare (type fixnum width height base-x base-y color24 color15))
             (setf width  (logand (+ (logand width #x3FF) #x00F) #x3F0))
             (setf height (logand height #x1FF))
             (setf base-x (logand base-x #x3F0))
             (setf base-y (logand base-y #x1FF))
             (dotimes (y height)
               (dotimes (x width)
                 (putpixel-unclamped this
                                     (+ base-x x)
                                     (+ base-y y)
                                     color15)))
             ))

          ((#x80)  ; TODO: Copy Rectangle VRAM to VRAM
           (when (< gp0-buffer-length 4)
             (return-from keep-gp0-buffer nil))
           (let* ((width  (logand #xFFFF (aref gp0-buffer 3)))
                  (height (ash (aref gp0-buffer 3) -16)))
             (declare (ignore width height))
             ;; TODO!
             ))

          ((#xA0)  ; TODO: Copy Rectangle CPU to VRAM
           (when (< gp0-buffer-length 3)
             (return-from keep-gp0-buffer nil))
           (with-slots (transfer-halfwords-in
                        transfer-width
                        transfer-height
                        transfer-dx
                        transfer-dy
                        transfer-reset-x
                        transfer-remaining-x) this

             (when (= gp0-buffer-length 3)
               (setf transfer-width   (1+ (logand #x03FF (1- (aref gp0-buffer 2)))))
               (setf transfer-height  (1+ (logand #x01FF (1- (ash (aref gp0-buffer 2) -16)))))
               (setf transfer-dx      (logand #x03FF (aref gp0-buffer 1)))
               (setf transfer-dy      (logand #x01FF (ash (aref gp0-buffer 1) -16)))
               (setf transfer-reset-x transfer-dx)
               (setf transfer-remaining-x transfer-width)
               (setf transfer-halfwords-in
                     (* transfer-width transfer-height))
               (assert (<= 0 transfer-dx (+ transfer-dx transfer-width -1) 1023))
               (assert (<= 0 transfer-dy (+ transfer-dy transfer-height -1) 511))
               )

             (when (= gp0-buffer-length 4)
               (dotimes (reps 2)
                 (when (> transfer-halfwords-in 0)
                   (setf (aref vram
                               (+ transfer-dx
                                  (* transfer-dy 1024)))
                         (logand #xFFFF (ash data (* -16 reps))))
                   (incf transfer-dx)
                   (decf transfer-remaining-x)
                   (when (<= transfer-remaining-x 0)
                     (setf transfer-dx transfer-reset-x)
                     (setf transfer-remaining-x transfer-width)
                     (incf transfer-dy))
                   (decf transfer-halfwords-in)))
               (decf gp0-buffer-length))

             (assert (= gp0-buffer-length 3))
             (unless (= transfer-halfwords-in 0)
               (assert (>= transfer-halfwords-in 0))
               (return-from keep-gp0-buffer nil))))

          ((#xC0)  ; TODO: Copy Rectangle VRAM to VRAM
           (when (< gp0-buffer-length 4)
             (return-from keep-gp0-buffer nil))
           (let* ((width  (logand #xFFFF (aref gp0-buffer 3)))
                  (height (ash (aref gp0-buffer 3) -16))
                  (sx     (logand #xFFFF (aref gp0-buffer 1)))
                  (sy     (ash (aref gp0-buffer 1) -16))
                  (dx     (logand #xFFFF (aref gp0-buffer 2)))
                  (dy     (ash (aref gp0-buffer 2) -16)))
             (declare (ignore width height))
             (declare (ignore sx sy dx dy))
             ;; TODO!
             ))

          ((#xE1)   ; Texpage
           (with-slots (global-texpage) this
             (setf global-texpage (logand command-word #x00FFFFFF))))
          ((#xE2) nil)   ; TODO: Texture window
          ((#xE3)   ; Drawing area xy1
           (with-slots (clamp-x1 clamp-y1) this
             (setf clamp-x1 (logand (ash command-word  -0) #x3FF))
             (setf clamp-y1 (logand (ash command-word -10) #x3FF)) ; 1024 high on newgpu
             ;(format t "1 ~a ~a~%" clamp-x1 clamp-y1)
             t))
          ((#xE4)   ; Drawing area xy2
           (with-slots (clamp-x2 clamp-y2) this
             (setf clamp-x2 (logand (ash command-word  -0) #x3FF))
             (setf clamp-y2 (logand (ash command-word -10) #x3FF)) ; 1024 high on newgpu
             ;(format t "2 ~a ~a~%" clamp-x2 clamp-y2)
             t))
          ((#xE5)   ; Drawing offset
           (with-slots (offs-x offs-y) this
             (setf offs-x (logand (ash command-word  -0) #x7FF))
             (setf offs-y (logand (ash command-word -11) #x7FF))
             (when (>= offs-x #x400) (decf offs-x #x800))
             (when (>= offs-y #x400) (decf offs-y #x800))
             t))
          ((#xE6) nil)   ; TODO: Mask bit
          )
        (setf gp0-buffer-length 0)))))

(defmethod gp1-reset-command-buffer ((this psgpu) data)
  (declare (inline))
  ;; TODO!
  nil)

(defmethod gp1-ack-irq1 ((this psgpu) data)
  (declare (inline))
  ;; TODO!
  nil)

(defmethod gp1-set-display-disable ((this psgpu) data)
  (declare (inline))
  ;; TODO!
  nil)

(defmethod gp1-set-dma-direction ((this psgpu) data)
  (declare (inline))
  ;; TODO!
  nil)

(defmethod gp1-set-display-vram-start ((this psgpu) data)
  (declare (inline))
  ;; TODO!
  ;; also do a flip
  (with-slots (vram disp-xbeg disp-ybeg
                    screen-x1 screen-y1
                    screen-x2 screen-y2
                    screen-pixel-width
                    screen-interlace-multiplier) this
    (sdl2:render-clear *screen-renderer*)
    (setf disp-xbeg (logand #x3FF data))
    (setf disp-ybeg (logand #x1FF (ash data -10)))
    (let* ((pixels (sdl2:lock-texture *screen-texture*)))
      (dotimes (i (* 1024 512))
        (setf (cffi:mem-aref pixels ':uint32 i)
              (the
                (unsigned-byte 32)
                (ash (the (unsigned-byte 32)
                          (color-15-to-24 (aref vram i))) 8)))))
    (sdl2:unlock-texture *screen-texture*)
    (sdl2:with-rects ((src-rect disp-xbeg
                                disp-ybeg
                                (floor (/ (- screen-x2 screen-x1 -1)
                                          screen-pixel-width))

                                ;; HACK: stabilise video for Spyro 3
                                ;; (to be replaced with something more appropriate)
                                (* 2 (floor (/ (- screen-y2 screen-y1 -1) 2))
                                   screen-interlace-multiplier)
                                ;(floor (/ (* 320 8)
                                ;          screen-pixel-width))
                                ;240
                                ))
      (sdl2:render-copy *screen-renderer*
                        *screen-texture*
                        :source-rect src-rect
                        ))
    (sdl2:render-present *screen-renderer*)
    nil))

(defmethod gp1-set-display-output-x-range ((this psgpu) data)
  (declare (inline))
  (with-slots (screen-x1 screen-x2) this
    (setf screen-x1 (logand #xFFF data))
    (setf screen-x2 (logand #xFFF (ash data -12))))
  t)

(defmethod gp1-set-display-output-y-range ((this psgpu) data)
  (declare (inline))
  (with-slots (screen-y1 screen-y2) this
    (setf screen-y1 (logand #x3FF data))
    (setf screen-y2 (logand #x3FF (ash data -10))))
  t)

(defmethod gp1-set-display-mode ((this psgpu) data)
  (declare (inline))
  (with-slots (screen-pixel-width
               screen-interlace-multiplier) this
    (setf screen-pixel-width
          (ecase (logand #x43 data)
            ((#x00) 10)
            ((#x01) 8)
            ((#x02) 5)
            ((#x03) 4)
            ((#x40 #x41 #x42 #x43) 9)))
    (setf screen-interlace-multiplier
          (ecase (logand #x24 data)
            ((#x00 #x04 #x20) 1)
            ((#x24) 2)))
    ;; TODO: the rest
    nil))

(defmethod gp1-reset-gpu ((this psgpu) data)
  (declare (inline))
  (write-gp1 this #x01000000)  ; Reset command buffer
  (write-gp1 this #x02000000)  ; Acknowledge IRQ1
  (write-gp1 this #x03000001)  ; Disable display
  (write-gp1 this #x04000000)  ; Disable DMA
  (write-gp1 this #x05000000)  ; Set display address to 0
  (write-gp1 this #x06C00200)  ; Set x range to 0x200, 0xC00 vclocks
  (write-gp1 this #x07040010)  ; Set y range to 0x010, 0x100
  (write-gp1 this #x08000001)  ; Display mode: 320x200, NTSC
  (write-gp0 this #xE1000000)  ; Texpage
  (write-gp0 this #xE2000000)  ; Texture window
  (write-gp0 this #xE3000000)  ; Drawing area xy1
  (write-gp0 this #xE4000000)  ; Drawing area xy2
  (write-gp0 this #xE5000000)  ; Drawing offset
  (write-gp0 this #xE6000000)  ; Mask bit
  t)

(defmethod write-gp1 ((this psgpu) data)
  (declare (type (unsigned-byte 32) data))
  ;(format t "GP1: ~8,'0X~%" data)
  (ecase (logand (ash data -24) #xFF)
    ((#x00) (gp1-reset-gpu this data))
    ((#x01) (gp1-reset-command-buffer this data))
    ((#x02) (gp1-ack-irq1 this data))
    ((#x03) (gp1-set-display-disable this data))
    ((#x04) (gp1-set-dma-direction this data))
    ((#x05) (gp1-set-display-vram-start this data))
    ((#x06) (gp1-set-display-output-x-range this data))
    ((#x07) (gp1-set-display-output-y-range this data))
    ((#x08) (gp1-set-display-mode this data))
    ((#x10) nil)  ; "Get GPU info" but this isn't useful yet
    ))

(sdl2:with-init (:everything)
  (sdl2:with-window (window :title "PlayStation 1 GPU Emulator"
                            :flags '(:shown)
                            ;:w 1024 :h 512
                            ;:w 2048 :h 1024
                            :w 1024 :h (* 3 224)
                            )
    (sdl2:with-renderer (renderer window :flags '(:accelerated))
      (with-open-file (file "spyro3gameplay.gpudump"
                            :direction :input
                            :element-type 'unsigned-byte)
        (let* ((psgpu (make-psgpu))
               (*screen-texture*
                 (sdl2:create-texture
                   renderer
                   ':bgrx8888
                   ':streaming
                   1024 512))
               (*screen-renderer* renderer)
               (*screen-window* window)
               (buffer-chunk-count 4096)
               (buffer-chunk-size 5)
               (gpu-data-buffer (make-array (* buffer-chunk-count
                                               buffer-chunk-size)
                                            :element-type 'unsigned-byte)))
          (labels ((/loop () 
                     (let* ((pos (read-sequence gpu-data-buffer file)))
                       (declare (type fixnum pos))
                       (when (/= pos 0)
                         (dotimes (idx (floor (/ pos 5)))
                           (/process (* idx 5)))
                         (when (= pos (length gpu-data-buffer))
                           (/loop)))))

                   (/process (offs)
                     (declare (type fixnum offs))
                     (declare (inline))
                     (let* ((addr (aref gpu-data-buffer (+ offs 0)))
                            (data (logior (ash (aref gpu-data-buffer (+ offs 1))  0)
                                          (ash (aref gpu-data-buffer (+ offs 2))  8)
                                          (ash (aref gpu-data-buffer (+ offs 3)) 16)
                                          (ash (aref gpu-data-buffer (+ offs 4)) 24))))
                       (declare (type fixnum addr data))
                       (if (= 0 (logand addr #x04))
                         (write-gp0 psgpu data)
                         (write-gp1 psgpu data)))))
            (/loop))))
      )))

