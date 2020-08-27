(lang dune 2.7)

(name vscode_jsoo)

(license MIT)

(source
 (github mnxn/vscode_jsoo))

(generate_opam_files true)

(package
 (name vscode_jsoo)
 (synopsis "Js_of_ocaml bindings to the VSCode Extension API")
 (depends js_of_ocaml gen_js_api))
