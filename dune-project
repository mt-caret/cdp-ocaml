(lang dune 3.20)

(name cdp_ocaml)

(generate_opam_files true)

(package
 (name cdp)
 (synopsis "Native OCaml client for the Chrome DevTools Protocol")
 (depends
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.4)))
  dune
  (core
   (and
    (>= v0.17.0)
    (< v0.18)))
  (core_kernel
   (and
    (>= v0.17.0)
    (< v0.18)))
  (bignum
   (and
    (>= v0.17.0)
    (< v0.18)))
  (async
   (and
    (>= v0.17.0)
    (< v0.18)))
  (jsonaf
   (and
    (>= v0.17.0)
    (< v0.18)))
  (ppx_jsonaf_conv
   (and
    (>= v0.17.0)
    (< v0.18)))
  (ppx_jane
   (and
    (>= v0.17.0)
    (< v0.18)))
  (cohttp-async
   (and
    (>= 5.1.1)
    (< 6.0.0)))
  (async_websocket
   (and
    (>= v0.17.0)
    (< v0.18)))
  (cohttp_async_websocket
   (and
    (>= v0.17.0)
    (< v0.18)))
  (base64
   (>= 3.5.0))
  uri))

(package
 (name cdp_screenshot)
 (synopsis "Screenshot demo for cdp_ocaml")
 (depends
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.4)))
  dune
  (cdp
   (= :version))
  (async
   (and
    (>= v0.17.0)
    (< v0.18)))
  (core
   (and
    (>= v0.17.0)
    (< v0.18)))
  (core_unix
   (and
    (>= v0.17.0)
    (< v0.18)))))
