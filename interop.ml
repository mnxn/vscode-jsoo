type 'a or_undefined = 'a option

type 'a maybe_list = 'a list

let undefined = Ojs.variable "undefined"

let or_undefined_of_js ml_of_js js_val =
  if js_val != undefined && js_val != Ojs.null then
    Some (ml_of_js js_val)
  else
    None

let or_undefined_to_js ml_to_js = function
  | Some ml_val -> ml_to_js ml_val
  | None        -> undefined

let maybe_list_of_js ml_of_js js_val =
  if js_val != undefined && js_val != Ojs.null then
    Ojs.list_of_js ml_of_js js_val
  else
    []

let maybe_list_to_js ml_to_js = function
  | [] -> undefined
  | xs -> Ojs.list_to_js ml_to_js xs

let iter_set obj field f value =
  Option.iter (fun value -> Ojs.set obj field (f value)) value

module Regexp = struct
  type t = Js_of_ocaml.Regexp.regexp

  let t_to_js : Js_of_ocaml.Regexp.regexp -> Ojs.t = Obj.magic

  let t_of_js : Ojs.t -> Js_of_ocaml.Regexp.regexp = Obj.magic
end

module Dict = struct
  type 'a t = (string, 'a) Hashtbl.t

  let t_to_js to_js tbl =
    let obj = Ojs.empty_obj () in
    let set k v = Ojs.set obj k (to_js v) in
    Hashtbl.iter set tbl;
    obj

  let t_of_js of_js obj =
    let tbl = Hashtbl.create 10 in
    let set k =
      let v = of_js (Ojs.get obj k) in
      Hashtbl.add tbl k v
    in
    Ojs.iter_properties obj set;
    tbl
end
