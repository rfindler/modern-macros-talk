#lang racket
(require drracket/private/standalone-module-browser
         racket/async-channel
         racket/runtime-path
         racket/serialize
         pkg/path setup/getinfo)
(provide deepest-pkg-paths)

(define (compute-deepest-pkg-paths)
  (define progress-channel (make-async-channel))
  (define connection-channel (make-async-channel))

  (define-values/invoke-unit process-program-unit
    (import process-program-import^)
    (export process-program-export^))

  (define drracket-starting-file (collection-file-path "tool-lib.rkt" "drracket"))
  (define tool-starting-files
    (for*/list ([info (in-list (find-relevant-directory-records '(drracket-tools tools)))]
                [file (in-list
                       (let ([table (get-info/full (directory-record-path info))])
                         (or (table 'drracket-tools (λ () #f))
                             (table 'tools (λ () '())))))])
      (match file
        [(list file) ;; why is this list here?
         (build-path (directory-record-path info) file)]
        [_ (build-path (directory-record-path info) file)])))
  (define starting-files (cons drracket-starting-file tool-starting-files))

  (void
   (thread
    (λ ()
      (for ([starting-file (in-list starting-files)])
        (add-connections drracket-starting-file))
      (async-channel-put connection-channel 'done))))

  (define path->pkg-cache (make-hash))

  (define edges (make-hash))

  (define (modname->path modname)
    (match modname
      [`(submod ,(? path? p) ,_ ...) p]
      [(? path? p) p]))

  (struct neighbor (pkg delta-depth) #:transparent)
  (define (add-edge src dest delta-depth)
    (define old (hash-ref edges src set))
    (define new (neighbor dest delta-depth))
    (hash-set! edges src (set-add old new)))
  (define (neighbors node) (hash-ref edges node set))

  (define (get-pkg-name modname)
    (define pkg (path->pkg (modname->path modname) #:cache path->pkg-cache))
    (cond
      [(not pkg) "base"]
      [(regexp-match #rx"^(.*)-lib$" pkg) => (λ (m) (list-ref m 1))]
      [else pkg]))
  
  (let loop ()
    (sync
     (handle-evt progress-channel (λ (x) (loop)))
     (handle-evt
      connection-channel
      (λ (x)
        (match x
          ['done
           (printf "done!\n")
           (void)]
          [(list file imports delta-depth)
           (add-edge file imports delta-depth)
           (loop)])))))

  (define depths (make-hash))

  (define file-paths (make-hash))
  (let ()
    (define visited (make-hash))
    (for ([starting-file (in-list starting-files)])
      (let loop ([file starting-file]
                 [depth 0]
                 [path '()])
        (unless (hash-ref visited (cons file depth) #f)
          (hash-set! visited (cons file depth) #t)
          (cond
            [(set-empty? (neighbors file))
             (define k (cons file depth))
             (hash-set! file-paths k (set-add (hash-ref file-paths k set) (cons (cons file depth) path)))]
            [else
             (for ([a-neighbor (in-set (neighbors file))])
               (match-define (neighbor neighbor-pkg delta-depth) a-neighbor)
               (when delta-depth
                 (loop neighbor-pkg
                       (+ depth delta-depth)
                       (cons (cons file depth) path))))])))))

  (define deepest-depth
    (for/fold ([n 0])
              ([(k v) (in-hash file-paths)])
      (max (cdr k) n)))

  (define (file-path->shrunken-pkg-path path)
    (let loop ([path path])
      (cond
        [(null? path) '()]
        [(null? (cdr path))
         (match-define (cons fst-path fst-depth) (car path))
         (define fst-pkg (get-pkg-name fst-path))
         (list (cons fst-pkg fst-depth))]
        [else
         (match-define (cons fst-path fst-depth) (car path))
         (match-define (cons snd-path snd-depth) (cadr path))
         (define fst-pkg (get-pkg-name fst-path))
         (define snd-pkg (get-pkg-name snd-path))
         (cond
           [(and (= fst-depth snd-depth) (equal? fst-pkg snd-pkg))
            (loop (cdr path))]
           [else
            (cons (cons fst-pkg fst-depth) (loop (cdr path)))])])))

  (define (file-path->pkg-path path)
    (let loop ([path path])
      (cond
        [(null? path) '()]
        [else
         (match-define (cons fst-path fst-depth) (car path))
         (define fst-pkg (get-pkg-name fst-path))
         (cons (list fst-pkg fst-depth fst-path) (loop (cdr path)))])))

  (define deepest-pkg-paths
    (for*/set ([(k v) (in-hash file-paths)]
               #:when (= (cdr k) deepest-depth)
               [path (in-set v)])
      (file-path->pkg-path path)))
  deepest-pkg-paths)

(define-runtime-path deepest-pkg-paths.rktd "deepest-pkg-paths.rktd")
(define (get-deepest-pkg-paths)
  (unless (file-exists? deepest-pkg-paths.rktd)
    (define deepest-pkg-paths (compute-deepest-pkg-paths))
    (call-with-output-file deepest-pkg-paths.rktd
      (λ (port)
        (write (serialize deepest-pkg-paths) port)
        (newline port))
      #:exists 'truncate))
  (deserialize (call-with-input-file deepest-pkg-paths.rktd read)))

(define deepest-pkg-paths (get-deepest-pkg-paths))
