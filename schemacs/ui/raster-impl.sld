(define-library (schemacs ui raster-impl)
  ;; The high-level Scheme API for raster graphics, sometimes referred
  ;; to as "pixel maps" or "bitmaps". The actual implementation of
  ;; displaying raster grahpics is platform dependent, these APIs
  ;; SHOULD work consistently on all platforms supported by Schemacs.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    (only (schemacs ui rectangle)
          size2D  size2D-type?  rect2D  rect2D-type?
          size2D-width  size2D-height  rect2D-point
          point2D  point2D-type?  point2D-x  point2D-y
          )
    )
  (export
   ;; A `raster` is an opaque data type for storing
   ;; implementation-specific raster data. There is also a
   ;; `raster-bytes-type?` record type which stores a raster image in
   ;; a Scheme bytevector along with an pixel format. The conversion
   ;; from a `raster` to a `raster-bytes-type?` is also implementation
   ;; specific as the `raster` data itself is.
   ;;-----------------------------------------------------------------
   raster-type?*  copy-raster*  blit-raster*
   raster->bytes*  bytes->raster*  raster-load*  raster-dump*
   svg->raster*
   ;;-----------------------------------------------------------------
   make<raster-bytes>  raster-bytes-type?
   raster-bytes-data  raster-bytes-size  raster-bytes-format
   ;;-----------------------------------------------------------------
   size2D  size2D-type?  rect2D  rect2D-type?
   size2D-width  size2D-height  rect2D-point
   point2D  point2D-type?  point2D-x  point2D-y
   ;;-----------------------------------------------------------------
   ;; Color formats
   pixel-format-type?  make<pixel-format-type>  pixel-format=
   pixel-format-channels-tuple  pixel-format-bit-depths-tuple
   pixel-format-bit-depth
   RGBA8  ARGB8  RGB8  ARGB4  RGBA4  RGB565  RGB332  RGB233
   YA8  AY8  Y8  Y4  Y2  AY1  YA1  Y1
   ;;-----------------------------------------------------------------
   ;; File formats
   JPEG PNG BMP
   )
  (begin

    (define-record-type <raster-bytes-type>
      (make<raster-bytes> pixform bytevec size)
      raster-bytes-type?
      (pixform  raster-bytes-format)
      (bytevec  raster-bytes-data)
      (size     raster-bytes-size)
      )

    (define raster-type?*
      ;; Returns `#t` only if the applied argument `RASTER` is an
      ;; implementation-specific opaque data type used to buffer
      ;; raster graphical data that can be projected onto a graphical
      ;; display.
      ;;--------------------------------------------------------------
      (make-parameter (lambda (raster) #f)))

    (define copy-raster*
      ;; Creates a full copy (not just a reference or a view of) the
      ;; pixels in the implementation-specific opaque data type of a
      ;; raster image `RASTER`. The `AREA` is a `rect2D` indicating
      ;; the domain of indicies of pixels of the `RASTER` to be
      ;; copied. Returns an opaque data structure of the same type as
      ;; `RASTER`. The size of the returned `RASTER` may be smaller
      ;; than the requested `AREA` argument if `AREA` was larger than
      ;; requested or partially out-of-bounds.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (raster area)
         (error "`copy-raster` not defined" raster area)
         )))

    (define blit-raster*
      ;; Copy to the `RASTER` at the given point2D `POINT` relative to
      ;; the origin point `(x=0, y=0)` of the `RASTER` from the source
      ;; `BUFFER`. The source can be restricted by a rect2D `AREA`
      ;; value, or can copy all available information in the source
      ;; buffer if `AREA` is #f.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (raster point source area)
         (error "`blit-raster` not defined" raster point source area)
         )))

    (define raster->bytes*
      ;; Convert the content of an implementation-specific opaque
      ;; `RASTER` data structure to a record type of
      ;; `raster-bytes-type?`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (raster)
         (error "`raster->bytes` not defined" raster)
         )))

    (define bytes->raster*
      ;; Convert a data structure of record type `raster-bytes-type?`
      ;; to an implementation-specific opaque `RASTER` data type.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (raster)
         (error "`raster->bytes` not defined" raster)
         )))

    (define raster-load*
      ;; When applied a given `FILEPATH` argument, load the raster
      ;; image from that `FILEPATH` and return an
      ;; implementation-specific `RASTER` data structure. The
      ;; `FILEPATH` argument should be of the same types that are used
      ;; by the Scheme procedures `open-binary-input-file`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (filepath)
         (error "`raster-load` not defined" filepath)
         )))

    (define raster-dump*
      ;; When applied a given `FILEPATH` argument, save the
      ;; implementation-specific `RASTER` data to that `FILEPATH`
      ;; using the given `FILE-FORMAT`, which is either `JPEG`, `PNG`,
      ;; `BMP`, or `#f` to indicate an implementation-specific default
      ;; file format. The `FILEPATH` argument should be of the same
      ;; types that are used by the Scheme procedures
      ;; `open-binary-output-file`.
      ;; --------------------------------------------------------------
      (make-parameter
       (lambda (raster file-format filepath)
         (error "`raster-load` not defined" raster file-format filepath)
         )))

    (define svg->raster*
      ;; Render SVG data to a raster image. The SVG data must have
      ;; already been constructed from the `(schemacs ui svg)` library.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (raster svg)
         (error "`svg->raster` not defined" raster svg)
         )))

    ;;----------------------------------------------------------------

    (define-record-type <pixel-format-type>
      (make<pixel-format> channels depth)
      pixel-format-type?
      (channels  pixel-format-channels-tuple)
      (depth     pixel-format-bit-depths-tuple)
      )

    (define pixel-format
      (case-lambda
        ((channels)
         (make<pixel-format> channels (make-bytevector (vector-length channels) 8))
         )
        ((channels depth)
         (make<pixel-format> channels depth)
         )))

    (define (pixel-format-bit-depth pf)
      ;; The sum of the bit depths of the `pixel-format-bit-depths-tuple`
      (let*((vec (pixel-format-bit-depths-tuple pf))
            (len (bytevector-length vec))
            )
        (let loop ((i 0) (sum 0))
          (cond
           ((< i len) (loop (+ 1 i) (+ sum (bytevector-u8-ref vec i))))
           (else sum)
           ))))

    (define (pixel-format= a b)
      (or (eq? a b)
          (and
           (equal?
            (pixel-format-channels-tuple a)
            (pixel-format-channels-tuple b)
            )
           (equal?
            (pixel-format-bit-depths-tuple a)
            (pixel-format-bit-depths-tuple b)
            ))))

    (define red 'red)
    (define green 'green)
    (define blue 'blue)
    (define alpha 'alpha)
    (define luminance 'luminance)
    (define rgb (vector red green blue))
    (define rgba (vector red green blue alpha))
    (define argb (vector alpha red green blue))

    (define RGBA8 (pixel-format rgba))
    (define ARGB8 (pixel-format argb))
    (define RGB8  (pixel-format rgb))
    (define ARGB4 (pixel-format argb #u8(4 4 4 4)))
    (define RGBA4 (pixel-format rgba #u8(4 4 4 4)))
    (define RGB565 (pixel-format rgb #u8(5 6 5)))
    (define RGB332 (pixel-format rgb #u8(3 3 2)))
    (define RGB233 (pixel-format rgb #u8(2 3 3)))

    ;; NOTE: the Y means "luminance" according to CIE-1931, so the "Y"
    ;; formats are grayscale.
    (define YA8 (pixel-format (vector luminance alpha) #u8(8 8)))
    (define Y8  (pixel-format (vector luminance) #u8(8)))
    (define AY8 (pixel-format (vector alpha luminance) #u8(8 8)))
    (define Y4  (pixel-format (vector luminance) #u8(4)))
    (define Y2  (pixel-format (vector luminance) #u8(2)))
    (define AY1 (pixel-format (vector alpha luminance) #u8(1 1)))
    (define YA1 (pixel-format (vector luminance alpha) #u8(1 1)))
    (define Y1  (pixel-format (vector luminance) #u8(1)))


    ;;----------------------------------------------------------------
    ))
