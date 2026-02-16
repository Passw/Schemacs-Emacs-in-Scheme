(define-library (schemacs ui raster)
  (import
    (scheme base)
    (scheme case-lambda)
    (schemacs ui raster-impl)
    )
  (export
   raster-type?  copy-raster  blit-raster
   raster->bytes  bytes->raster  raster-load  raster-dump
   svg->raster
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
    (define (raster-type? . args) (apply (raster-type?*) args))
    (define (copy-raster . args) (apply (copy-raster*) args))
    (define (blit-raster . args) (apply (blit-raster*) args))
    (define (raster->bytes . args) (apply (raster->bytes*) args))
    (define (bytes->raster . args) (apply (bytes->raster*) args))
    (define (raster-load . args) (apply (raster-load*) args))
    (define (raster-dump . args) (apply (raster-dump*) args))
    (define (svg->raster . args) (apply (svg->raster*) args))
    ))
