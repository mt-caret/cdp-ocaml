(lang dune 3.22)

(name cdp_ocaml)

(generate_opam_files true)

(package
 (name cdp)
 (synopsis "Native OCaml client for the Chrome DevTools Protocol")
 (depends
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3)))
  dune
  (core
   (and
    (>= v0.17.0)
    (< v0.19)))
  (core_kernel
   (and
    (>= v0.17.0)
    (< v0.19)))
  (bignum
   (and
    (>= v0.17.0)
    (< v0.19)))
  (async
   (and
    (>= v0.17.0)
    (< v0.19)))
  (jsonaf
   (and
    (>= v0.17.0)
    (< v0.19)))
  (ppx_jsonaf_conv
   (and
    (>= v0.17.0)
    (< v0.19)))
  (ppx_jane
   (and
    (>= v0.17.0)
    (< v0.19)))
  (cohttp-async
   (and
    (>= 5.1.1)
    (< 6.0.0)))
  (async_websocket
   (and
    (>= v0.17.0)
    (< v0.19)))
  (cohttp_async_websocket
   (and
    (>= v0.17.0)
    (< v0.19)))
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
    (< 5.3)))
  dune
  (cdp
   (= :version))
  (async
   (and
    (>= v0.17.0)
    (< v0.19)))
  (core
   (and
    (>= v0.17.0)
    (< v0.19)))
  (core_unix
   (and
    (>= v0.17.0)
    (< v0.19)))))
