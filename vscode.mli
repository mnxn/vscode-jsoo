[@@@js.stop]

type 'a or_undefined = 'a option

type regexp = Js_of_ocaml.Regexp.regexp

[@@@js.start]

[@@@js.implem
type 'a or_undefined = 'a option

external equals : Ojs.t -> Ojs.t -> bool = "caml_js_equals"

external pure_js_expr : string -> Ojs.t = "caml_pure_js_expr"

let undefined = pure_js_expr "undefined"

let or_undefined_of_js f x =
  if equals x undefined then
    None
  else
    Some (f x)

let or_undefined_to_js f = function
  | Some x -> f x
  | None   -> undefined

type regexp = Js_of_ocaml.Regexp.regexp

let regexp_to_js : regexp -> Ojs.t = Obj.magic

let regexp_of_js : Ojs.t -> regexp = Obj.magic

let () =
  let open Js_of_ocaml.Js in
  let require = Unsafe.get Unsafe.global "require" in
  let vscode = Unsafe.fun_call require [| Unsafe.inject (string "vscode") |] in
  Unsafe.set Unsafe.global "vscode" vscode]

val get_version : unit -> string [@@js.get "vscode.version"]

module Disposable : sig
  type t = private Ojs.t

  val from : (t list[@js.variadic]) -> t [@@js.global]

  val make : dispose:(unit -> unit) -> unit -> t [@@js.new "Disposable"]

  val dispose : t -> unit -> unit [@@js.call]
end [@js.scope "vscode.Disposable"]

module Command : sig
  type t =
    { title : string
    ; command : string
    ; tooltip : string or_undefined
    ; arguments : Ojs.t array or_undefined
    }

  val create :
       title:string
    -> command:string
    -> ?tooltip:string
    -> ?arguments:Ojs.t array
    -> unit
    -> t
    [@@js.builder]
end

module Position : sig
  type t = private
    { line : int
    ; character : int
    }

  type this = t

  val make : line:int -> character:int -> unit -> t [@@js.new "Position"]

  val is_before : this -> t -> bool [@@js.call]

  val is_before_or_equal : this -> t -> bool [@@js.call]

  val is_after : this -> t -> bool [@@js.call]

  val is_after_or_equal : this -> t -> bool [@@js.call]

  val is_equal : this -> t -> bool [@@js.call]

  val compare_to : this -> t -> int [@@js.call]

  val translate : this -> ?line_delta:int -> ?character_delta:int -> unit -> t
    [@@js.call]

  val with_ : this -> ?line:int -> ?character:int -> unit -> t [@@js.call]
end

module Range : sig
  type t = private
    { start : Position.t
    ; end_ : Position.t
    ; is_empty : bool
    ; is_single_line : bool
    }

  type this = t

  val from_positions : start:Position.t -> end_:Position.t -> unit -> t
    [@@js.new "vscode.Range"]

  val from_coordinates :
       start_line:int
    -> start_character:int
    -> end_line:int
    -> end_character:int
    -> unit
    -> t
    [@@js.new "vscode.Range"]

  val contains :
    this -> ([ `Position of Position.t | `Range of t ][@js.union]) -> bool
    [@@js.call]

  val is_equal : this -> t -> bool [@@js.call]

  val intersection : this -> t -> t or_undefined [@@js.call]

  val union : this -> t -> t [@@js.call]

  val with_ : this -> ?start:Position.t -> ?end_:Position.t -> unit -> t
    [@@js.call]
end

module TextLine : sig
  type t =
    { line_number : int
    ; text : string
    ; range : Range.t
    ; range_including_line_break : Range.t
    ; first_non_whitespace_character_index : int
    ; is_empty_or_whitespace : bool
    }

  val create :
       line_number:int
    -> text:string
    -> range:Range.t
    -> range_including_line_break:Range.t
    -> first_non_whitespace_character_index:int
    -> is_empty_or_whitespace:bool
    -> unit
    -> t
    [@@js.builder]
end

module EndOfLine : sig
  type t =
    | CRLF [@js 2]
    | LF [@js 1]
  [@@js.enum]
end

module TextEdit : sig
  type t = private
    { range : Range.t
    ; new_text : string
    ; new_eol : EndOfLine.t or_undefined
    }

  val replace : range:Range.t -> new_text:string -> unit -> t [@@js.global]

  val insert : position:Position.t -> new_text:string -> unit -> t [@@js.global]

  val delete : Range.t -> t [@@js.global]

  val set_end_of_line : EndOfLine.t -> t [@@js.global]

  val create : range:Range.t -> new_text:string -> unit -> t [@@js.builder]
end [@js.scope "vscode.TextEdit"]

module Uri : sig
  type t = private
    { scheme : string
    ; authority : string
    ; path : string
    ; query : string
    ; fragment : string
    ; fs_path : string
    }

  type this = t

  val parse : string -> ?strict:bool -> unit -> t [@@js.global]

  val file : string -> t [@@js.global]

  val join_path : t -> (string list[@js.variadic]) -> t [@@js.global]

  (* method with_ :
        ?scheme:string
     -> ?authority:string
     -> ?path:string
     -> ?query:string
     -> ?fragment:string
     -> unit
     -> t *)

  val to_string : this -> ?skip_encoding:bool -> unit -> string

  val to_json : this -> unit -> Ojs.t
end [@js.scope "vscode.Uri"]

module TextDocument : sig
  type t =
    { uri : Uri.t
    ; file_name : string
    ; is_untitled : bool
    ; language_id : string
    ; version : int
    ; is_dirty : bool
    ; is_closed : bool
    ; eol : EndOfLine.t
    ; line_count : int
    }

  type this = t

  (* val save : this -> unit -> bool Promise.t *)
  val line_at : this -> int -> TextLine.t [@@js.call]

  val line_at_position : this -> Position.t -> TextLine.t [@@js.call "lineAt"]

  val offset_at : this -> Position.t -> int [@@js.call]

  val position_at : this -> int -> Position.t [@@js.call]

  val get_text : this -> ?range:Range.t -> unit -> string [@@js.call]

  val get_word_range_at_position :
    this -> Position.t -> ?regex:regexp -> unit -> Range.t or_undefined
    [@@js.call]

  val validate_range : this -> Range.t -> Range.t [@@js.call]

  val validate_position : this -> Position.t -> Position.t [@@js.call]
end
