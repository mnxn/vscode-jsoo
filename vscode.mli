[@@@js.stop]

type regexp = Js_of_ocaml.Regexp.regexp

[@@@js.start]

[@@@js.implem
type regexp = Js_of_ocaml.Regexp.regexp

let regexp_to_js : regexp -> Ojs.t = Obj.magic

let regexp_of_js : Ojs.t -> regexp = Obj.magic]

val get_version : unit -> string [@@js.get "vscode.version"]

module Disposable : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method dispose : unit -> unit
       end

  val make : dispose:(unit -> unit) -> unit -> t [@@js.new "Disposable"]

  val from : (t list[@js.variadic]) -> t [@@js.global "Disposable.from"]
end [@scope "vscode"]

module Command : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method title : string

         method command : string

         method tooltip : string option

         method arguments : Ojs.t array option
       end

  val create :
       title:string
    -> command:string
    -> ?tooltip:string
    -> ?arguments:Ojs.t array
    -> unit
    -> t
    [@@js.builder]
end [@scope "vscode"]

module Position : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method line : int

         method character : int

         method is_before : t -> bool

         method is_before_or_equal : t -> bool

         method is_after : t -> bool

         method is_after_or_equal : t -> bool

         method is_equal : t -> bool

         method compare_to : t -> int

         method translate : ?line_delta:int -> ?character_delta:int -> unit -> t

         method with_ : ?line:int -> ?character:int -> unit -> t
       end

  val make : line:int -> character:int -> unit -> t [@@js.new "Position"]
end [@scope "vscode"]

module Range : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method start : Position.t

         method end_ : Position.t

         method is_empty : bool

         method is_single_line : bool

         method contains :
           ([ `Position of Position.t | `Range of t ][@js.union]) -> bool

         method is_equal : t -> bool

         method intersection : t -> t option

         method union : t -> t

         method with_ : ?start:Position.t -> ?end_:Position.t -> unit -> t
       end

  val from_positions : start:Position.t -> end_:Position.t -> unit -> t
    [@@js.new "Range"]

  val from_coordinates :
       start_line:int
    -> start_character:int
    -> end_line:int
    -> end_character:int
    -> unit
    -> t
    [@@js.new "Range"]
end [@scope "vscode"]

module TextLine : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method line_number : int

         method text : string

         method range : Range.t

         method range_including_line_break : Range.t

         method first_non_whitespace_character_index : int

         method is_empty_or_whitespace : bool
       end

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
end [@scope "vscode"]

module EndOfLine : sig
  type t =
    | CRLF [@js 2]
    | LF [@js 1]
  [@@js.enum]
end [@scope "vscode"]

module TextEdit : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method range : Range.t

         method new_text : string

         method new_eol : EndOfLine.t
       end

  val create : range:Range.t -> new_text:string -> unit -> t [@@js.builder]

  val replace : range:Range.t -> new_text:string -> unit -> t
    [@@js.global "TextEdit.replace"]

  val insert : position:Position.t -> new_text:string -> unit -> t
    [@@js.global "TextEdit.insert"]

  val delete : Range.t -> t [@@js.global "TextEdit.delete"]

  val set_end_of_line : EndOfLine.t -> t [@@js.global "TextEdit.setEndOfLine"]
end [@scope "vscode"]

module Uri : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method scheme : string

         method authority : string

         method path : string

         method query : string

         method fragment : string

         method fs_path : string

         method to_string : ?skip_encoding:bool -> unit -> string

         method to_json : unit -> Ojs.t
         (* method with_ :
               ?scheme:string
            -> ?authority:string
            -> ?path:string
            -> ?query:string
            -> ?fragment:string
            -> unit
            -> t *)
       end

  val parse : string -> ?strict:bool -> unit -> t [@@js.global "Uri.parse"]

  val file : string -> t [@@js.global "Uri.parse"]

  val join_path : t -> (string list[@js.variadic]) -> t
    [@@js.global "Uri.joinPath"]
end [@scope "vscode"]

module TextDocument : sig
  class t :
    Ojs.t
    -> object
         inherit Ojs.obj

         method uri : Uri.t

         method file_name : string

         method is_untitled : bool

         method language_id : string

         method version : int

         method is_dirty : bool

         method is_closed : bool

         method eol : EndOfLine.t

         method line_count : int

         (* method save : unit -> bool Promise.t *)
         method line_at : int -> TextLine.t

         method line_at_position : Position.t -> TextLine.t
         [@@js.call "lineAt"]

         method offset_at : Position.t -> int

         method position_at : int -> Position.t

         method get_text : ?range:Range.t -> unit -> string

         method get_word_range_at_position :
           Position.t -> ?regex:regexp -> unit -> Range.t option

         method validate_range : Range.t -> Range.t

         method validate_position : Position.t -> Position.t
       end
end [@scope "vscode"]
