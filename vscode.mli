[@@@js.stop]

type 'a or_undefined = 'a option

[@@@js.start]

[@@@js.implem
type 'a or_undefined = 'a option

let undefined = Ojs.variable "undefined"

let or_undefined_of_js ml_of_js js_val =
  if js_val != undefined && js_val != Ojs.null then
    Some (ml_of_js js_val)
  else
    None

let or_undefined_to_js ml_to_js = function
  | Some ml_val -> ml_to_js ml_val
  | None        -> undefined

let iter_set obj field f value =
  Option.iter (fun value -> Ojs.set obj field (f value)) value]

module Regexp : sig
  [@@@js.stop]

  type t = Js_of_ocaml.Regexp.regexp

  [@@@js.start]

  [@@@js.implem
  type t = Js_of_ocaml.Regexp.regexp

  let t_to_js : Js_of_ocaml.Regexp.regexp -> Ojs.t = Obj.magic

  let t_of_js : Ojs.t -> Js_of_ocaml.Regexp.regexp = Obj.magic]
end

module Dict : sig
  [@@@js.stop]

  type 'a t = (string, 'a) Hashtbl.t

  [@@@js.start]

  [@@@js.implem
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
    tbl]
end

module Disposable : sig
  type t = private (* class *) Ojs.t

  val from : (t list[@js.variadic]) -> t [@@js.global]

  val make : dispose:(unit -> unit) -> t [@@js.new "vscode.Disposable"]

  val dispose : t -> unit [@@js.call]
end [@js.scope "vscode.Disposable"]

module Command : sig
  type t = private (* interface *) Ojs.t

  val title : t -> string [@@js.get]

  val command : t -> string [@@js.get]

  val tooltip : t -> string or_undefined [@@js.get]

  val arguments : t -> Ojs.t list or_undefined [@@js.get]

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
  type t = private (* class *) Ojs.t

  val line : t -> int [@@js.get]

  val character : t -> int [@@js.get]

  val make : line:int -> character:int -> t [@@js.new "vscode.Position"]

  val is_before : t -> other:t -> bool [@@js.call]

  val is_before_or_equal : t -> other:t -> bool [@@js.call]

  val is_after : t -> other:t -> bool [@@js.call]

  val is_after_or_equal : t -> other:t -> bool [@@js.call]

  val is_equal : t -> other:t -> bool [@@js.call]

  val compare_to : t -> other:t -> int [@@js.call]

  val translate : t -> ?line_delta:int -> ?character_delta:int -> unit -> t
    [@@js.call]

  val with_ : t -> ?line:int -> ?character:int -> unit -> t [@@js.call]
end

module Range : sig
  type t = private (* class *) Ojs.t

  val start : t -> Position.t [@@js.get]

  val end_ : t -> Position.t [@@js.get]

  val make_positions : start:Position.t -> end_:Position.t -> t
    [@@js.new "vscode.Range"]

  val make_coordinates :
       start_line:int
    -> start_character:int
    -> end_line:int
    -> end_character:int
    -> t
    [@@js.new "vscode.Range"]

  val is_empty : t -> bool [@@js.get]

  val is_single_line : t -> bool [@@js.get]

  val contains :
       t
    -> position_or_range:([ `Position of Position.t | `Range of t ][@js.union])
    -> bool
    [@@js.call]

  val is_equal : t -> other:t -> bool [@@js.call]

  val intersection : t -> range:t -> t or_undefined [@@js.call]

  val union : t -> other:t -> t [@@js.call]

  val with_ : t -> ?start:Position.t -> ?end_:Position.t -> unit -> t
    [@@js.call]
end

module TextLine : sig
  type t = private (* interface *) Ojs.t

  val line_number : t -> int [@@js.get]

  val text : t -> string [@@js.get]

  val range : t -> Range.t [@@js.get]

  val range_including_line_break : t -> Range.t [@@js.get]

  val first_non_whitespace_character_index : t -> int [@@js.get]

  val is_empty_or_whitespace : t -> bool [@@js.get]

  val create :
       line_number:int
    -> text:string
    -> range:Range.t
    -> range_including_line_break:Range.t
    -> first_non_whitespace_character_index:int
    -> is_empty_or_whitespace:bool
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
  type t = private (* class *) Ojs.t

  val replace : range:Range.t -> new_text:string -> t [@@js.global]

  val insert : position:Position.t -> new_text:string -> t [@@js.global]

  val delete : Range.t -> t [@@js.global]

  val set_end_of_line : EndOfLine.t -> t [@@js.global]

  val range : t -> Range.t [@@js.get]

  val new_text : t -> string [@@js.get]

  val new_eol : t -> EndOfLine.t or_undefined [@@js.get]

  val make : range:Range.t -> new_text:string -> t [@@js.new "vscode.TextEdit"]
end [@js.scope "vscode.TextEdit"]

module Uri : sig
  type t = private (* class *) Ojs.t

  val parse : string -> ?strict:bool -> unit -> t [@@js.global]

  val file : string -> t [@@js.global]

  val join_path : t -> path_segments:(string list[@js.variadic]) -> t
    [@@js.global]

  val scheme : t -> string [@@js.get]

  val authority : t -> string [@@js.get]

  val path : t -> string [@@js.get]

  val query : t -> string [@@js.get]

  val fragment : t -> string [@@js.get]

  val fs_path : t -> string [@@js.get]

  val with_ :
       t
    -> ?scheme:string
    -> ?authority:string
    -> ?path:string
    -> ?query:string
    -> ?fragment:string
    -> unit
    -> t
    [@@js.custom
      val with_ : t -> Ojs.t -> t [@@js.call]

      let with_ this ?scheme ?authority ?path ?query ?fragment () =
        let change = Ojs.obj [||] in
        iter_set change "scheme" Ojs.string_to_js scheme;
        iter_set change "authority" Ojs.string_to_js authority;
        iter_set change "path" Ojs.string_to_js path;
        iter_set change "query" Ojs.string_to_js query;
        iter_set change "fragment" Ojs.string_to_js fragment;
        with_ this change]

  val to_string : t -> ?skip_encoding:bool -> unit -> string

  val to_json : t -> Jsonoo.t
end [@js.scope "vscode.Uri"]

module TextDocument : sig
  type t = private (* interface *) Ojs.t

  val uri : t -> Uri.t [@@js.get]

  val file_name : t -> string [@@js.get]

  val is_untitled : t -> bool [@@js.get]

  val language_id : t -> string [@@js.get]

  val version : t -> int [@@js.get]

  val is_dirty : t -> bool [@@js.get]

  val is_closed : t -> bool [@@js.get]

  val save : t -> bool Promise.t [@@js.call]

  val eol : t -> EndOfLine.t [@@js.get]

  val line_count : t -> int [@@js.get]

  val line_at : t -> line:int -> TextLine.t [@@js.call]

  val line_at_position : t -> position:Position.t -> TextLine.t
    [@@js.call "lineAt"]

  val offset_at : t -> position:Position.t -> int [@@js.call]

  val position_at : t -> offset:int -> Position.t [@@js.call]

  val get_text : t -> ?range:Range.t -> unit -> string [@@js.call]

  val get_word_range_at_position :
    t -> position:Position.t -> ?regex:Regexp.t -> unit -> Range.t or_undefined
    [@@js.call]

  val validate_range : t -> range:Range.t -> Range.t [@@js.call]

  val validate_position : t -> position:Position.t -> Position.t [@@js.call]
end

module WorkspaceFolder : sig
  type t = private (* interface *) Ojs.t

  val uri : t -> Uri.t [@@js.get]

  val name : t -> string [@@js.get]

  val index : t -> int [@@js.get]

  val create : uri:Uri.t -> name:string -> index:int -> t [@@js.builder]
end

module ViewColumn : sig
  type t =
    | Active [@js -1]
    | Beside [@js -2]
    | One [@js 1]
    | Two [@js 2]
    | Three [@js 3]
    | Four [@js 4]
    | Five [@js 5]
    | Six [@js 6]
    | Seven [@js 7]
    | Eight [@js 8]
    | Nine [@js 9]
  [@@js.enum]
end

module Selection : sig
  type t = private (* class extends *) Range.t

  val anchor : t -> Position.t [@@js.get]

  val active : t -> Position.t [@@js.get]

  val make_positions : anchor:Position.t -> active:Position.t -> t
    [@@js.new "vscode.Selection"]

  val make_coordinates :
       anchor_line:int
    -> anchor_character:int
    -> active_line:int
    -> active_character:int
    -> t
    [@@js.new "vscode.Selection"]

  val is_reversed : t -> bool [@@js.get]
end

module TextEditorEdit : sig
  type t = private (* interface *) Ojs.t

  type replace_location =
    ([ `Position of Position.t
     | `Range of Range.t
     | `Selection of Selection.t
     ]
    [@js.union])

  [@@@js.implem
  let replace_location_of_js js_val =
    if Ojs.has_property js_val "anchor" then
      `Position (Selection.t_of_js js_val)
    else if Ojs.has_property js_val "start" then
      `Range (Range.t_of_js js_val)
    else
      `Selection (Selection.t_of_js js_val)]

  type delete_location =
    ([ `Range of Range.t
     | `Selection of Selection.t
     ]
    [@js.union])

  [@@@js.implem
  let delete_location_of_js js_val =
    if Ojs.has_property js_val "anchor" then
      `Selection (Selection.t_of_js js_val)
    else
      `Range (Range.t_of_js js_val)]

  val replace : t -> location:replace_location -> value:string -> unit
    [@@js.call]

  val insert : t -> location:Position.t -> value:string -> unit [@@js.call]

  val delete : t -> location:delete_location -> unit [@@js.call]

  val set_end_of_line : t -> end_of_line:EndOfLine.t -> t [@@js.call]

  val create :
       replace:(location:replace_location -> value:string -> unit)
    -> insert:(location:Position.t -> value:string -> unit)
    -> delete:(location:delete_location -> unit)
    -> set_end_of_line:(end_of_line:EndOfLine.t -> t)
    -> t
    [@@js.builder]
end

module TextEditorCursorStyle : sig
  type t =
    | Line [@js 1]
    | Block [@js 2]
    | Underline [@js 3]
    | Line_thin [@js 4]
    | Block_outline [@js 5]
    | Underline_thin [@js 6]
  [@@js.enum]
end

module TextEditorLineNumbersStyle : sig
  type t =
    | Off [@js 0]
    | On [@js 1]
    | Relative [@js 2]
  [@@js.enum]
end

module TextEditorRevealType : sig
  type t =
    | Default [@js 0]
    | In_center [@js 1]
    | In_center_if_outside_viewport [@js 2]
    | At_top [@js 3]
  [@@js.enum]
end

module TextEditorOptions : sig
  type t = private (* interface *) Ojs.t

  type tab_size =
    ([ `Int of int
     | `String of string
     ]
    [@js.union])

  [@@@js.implem
  let tab_size_of_js js_val =
    match Ojs.type_of js_val with
    | "number" -> `Int (Ojs.int_of_js js_val)
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> assert false]

  type insert_spaces =
    ([ `Bool of bool
     | `String of string
     ]
    [@js.union])

  [@@@js.implem
  let insert_spaces_of_js js_val =
    match Ojs.type_of js_val with
    | "boolean" -> `Bool (Ojs.bool_of_js js_val)
    | "string"  -> `String (Ojs.string_of_js js_val)
    | _         -> assert false]

  val tab_size : t -> tab_size or_undefined [@@js.get]

  val insert_spaces : t -> insert_spaces or_undefined [@@js.get]

  val cursor_style : t -> TextEditorCursorStyle.t or_undefined [@@js.get]

  val line_numbers : t -> TextEditorLineNumbersStyle.t or_undefined [@@js.get]

  val create :
       ?tab_size:tab_size
    -> ?insert_spaces:insert_spaces
    -> ?cursor_style:TextEditorCursorStyle.t
    -> ?line_numbers:TextEditorLineNumbersStyle.t
    -> unit
    -> t
    [@@js.builder]
end

module TextEditorDecorationType : sig
  type t = private (* interface *) Ojs.t

  val key : t -> string [@@js.get]

  val dispose : t -> unit [@@js.call]

  val disposable : t -> Disposable.t
    [@@js.custom
      let disposable this = Disposable.make ~dispose:(fun () -> dispose this)]

  val create : key:string -> dispose:(unit -> unit) -> t [@@js.builder]
end

module MarkdownString : sig
  type t = private (* class *) Ojs.t

  val value : t -> string [@@js.get]

  val is_trusted : t -> bool or_undefined [@@js.get]

  val support_theme_icons : t -> bool or_undefined [@@js.get]

  val make : ?value:string -> ?support_theme_icons:bool -> unit -> t
    [@@js.new "vscode.MarkdownString"]

  val append_text : t -> value:string -> t [@@js.call]

  val append_markdown : t -> value:string -> t [@@js.call]

  val append_codeblock : t -> value:string -> ?language:string -> unit -> t
    [@@js.call]
end

module ThemeColor : sig
  type t = private (* class *) Ojs.t

  val make : id:string -> t [@@js.new "vscode.ThemeColor"]
end

module ThemableDecorationAttachmentRenderOptions : sig
  type t = private (* interface *) Ojs.t

  type content_icon_path =
    ([ `String of string
     | `Uri of Uri.t
     ]
    [@js.union])

  [@@@js.implem
  let content_icon_path_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `Uri (Uri.t_of_js js_val)]

  type color =
    ([ `String of string
     | `ThemeColor of ThemeColor.t
     ]
    [@js.union])

  [@@@js.implem
  let color_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `ThemeColor (ThemeColor.t_of_js js_val)]

  val content_text : t -> string or_undefined [@@js.get]

  val content_icon_path : t -> content_icon_path or_undefined [@@js.get]

  val border : t -> string or_undefined [@@js.get]

  val border_color : t -> color or_undefined [@@js.get]

  val font_style : t -> string or_undefined [@@js.get]

  val font_weight : t -> string or_undefined [@@js.get]

  val text_decoration : t -> string or_undefined [@@js.get]

  val color : t -> color or_undefined [@@js.get]

  val background_color : t -> color or_undefined [@@js.get]

  val margin : t -> string or_undefined [@@js.get]

  val width : t -> string or_undefined [@@js.get]

  val height : t -> string or_undefined [@@js.get]

  val create :
       ?content_text:string
    -> ?content_icon_path:content_icon_path
    -> ?border:string
    -> ?border_color:color
    -> ?font_style:string
    -> ?font_weight:string
    -> ?text_decoration:string
    -> ?color:color
    -> ?background_color:color
    -> ?margin:string
    -> ?width:string
    -> ?height:string
    -> unit
    -> t
    [@@js.builder]
end

module ThemableDecorationInstanceRenderOptions : sig
  type t = private (* interface *) Ojs.t

  val before : t -> ThemableDecorationAttachmentRenderOptions.t or_undefined
    [@@js.get]

  val after : t -> ThemableDecorationAttachmentRenderOptions.t or_undefined
    [@@js.get]

  val create :
       ?before:ThemableDecorationAttachmentRenderOptions.t
    -> ?after:ThemableDecorationAttachmentRenderOptions.t
    -> unit
    -> t
    [@@js.builder]
end

module DecorationInstanceRenderOptions : sig
  type t = private (* interface *) Ojs.t

  val light : t -> ThemableDecorationInstanceRenderOptions.t or_undefined
    [@@js.get]

  val dark : t -> ThemableDecorationInstanceRenderOptions.t or_undefined
    [@@js.get]

  val create :
       ?light:ThemableDecorationInstanceRenderOptions.t
    -> ?dark:ThemableDecorationInstanceRenderOptions.t
    -> unit
    -> t
    [@@js.builder]
end

module DecorationOptions : sig
  type t = private (* interface *) Ojs.t

  type hover_message =
    ([ `MarkdownString of MarkdownString.t
     | `MarkdownStrings of MarkdownString.t list
     ]
    [@js.union])

  [@@@js.implem
  let hover_message_of_js js_val =
    if Ojs.has_property js_val "value" then
      `MarkdownString (MarkdownString.t_of_js js_val)
    else
      `MarkdownStrings (Ojs.list_of_js MarkdownString.t_of_js js_val)]

  val range : t -> Range.t [@@js.get]

  val hover_message : t -> hover_message or_undefined [@@js.get]

  val render_options : t -> DecorationInstanceRenderOptions.t or_undefined
    [@@js.get]

  val create :
       range:Range.t
    -> ?hover_message:hover_message
    -> ?render_options:DecorationInstanceRenderOptions.t or_undefined
    -> unit
    -> t
    [@@js.builder]
end

module SnippetString : sig
  type t = private (* class *) Ojs.t

  val value : t -> string [@@js.get]

  val make : ?value:string -> unit -> t [@@js.new "vscode.SnippetString"]

  val append_text : t -> string:string -> t [@@js.call]

  val append_tab_stop : t -> number:int -> t [@@js.call]

  val append_place_holder :
       t
    -> value:([ `String of string | `Function of t -> unit ][@js.union])
    -> ?number:int
    -> unit
    -> t
    [@@js.call]

  val append_choice : t -> values:string list -> ?number:int -> unit -> t
    [@@js.call]

  val append_variable :
       t
    -> name:string
    -> default_value:([ `String of string | `Function of t -> unit ][@js.union])
    -> t
    [@@js.call]
end

module TextEditor : sig
  type t = private (* interface*) Ojs.t

  type insert_snippet_location =
    ([ `Position of Position.t
     | `Range of Range.t
     | `Positions of Position.t list
     | `Ranges of Range.t list
     ]
    [@js.union])

  val document : t -> TextDocument.t [@@js.get]

  val selection : t -> Selection.t [@@js.get]

  val selections : t -> Selection.t list [@@js.get]

  val visible_ranges : t -> Range.t list [@@js.get]

  val options : t -> TextEditorOptions.t [@@js.get]

  val view_column : t -> ViewColumn.t or_undefined [@@js.get]

  val edit :
       t
    -> callback:(TextEditorEdit.t -> unit)
    -> ?undo_stop_before:bool
    -> ?undo_stop_after:bool
    -> unit
    -> bool Promise.t
    [@@js.custom
      val edit :
           t
        -> callback:(TextEditorEdit.t -> unit)
        -> Ojs.t
        -> unit
        -> bool Promise.t
        [@@js.call]

      let edit this ~callback ?undo_stop_before ?undo_stop_after () =
        let options = Ojs.obj [||] in
        iter_set options "undoStopBefore" Ojs.bool_to_js undo_stop_before;
        iter_set options "undoStopAfter" Ojs.bool_to_js undo_stop_after;
        edit this ~callback options ()]

  val insert_snippet :
       t
    -> snippet:SnippetString.t
    -> ?location:insert_snippet_location
    -> ?undo_stop_before:bool
    -> ?undo_stop_after:bool
    -> unit
    -> bool Promise.t
    [@@js.custom
      val insert_snippet :
           t
        -> snippet:SnippetString.t
        -> ?location:insert_snippet_location
        -> Ojs.t
        -> bool Promise.t
        [@@js.call]

      let insert_snippet this ~snippet ?location ?undo_stop_before
          ?undo_stop_after () =
        let options = Ojs.obj [||] in
        iter_set options "undoStopBefore" Ojs.bool_to_js undo_stop_before;
        iter_set options "undoStopAfter" Ojs.bool_to_js undo_stop_after;
        insert_snippet this ~snippet ?location options]

  val set_decorations :
       t
    -> decoration_type:TextEditorDecorationType.t
    -> ranges_or_options:
         ([ `Ranges of Range.t list | `Options of DecorationOptions.t list ]
         [@js.union])
    -> unit
    [@@js.call]

  val reveal_range :
    t -> range:Range.t -> ?reveal_type:TextEditorRevealType.t -> unit -> unit
    [@@js.call]
end

module ConfigurationTarget : sig
  type t =
    | Global [@js 1]
    | Workspace [@js 2]
    | Workspace_folder [@js 3]
  [@@js.enum]
end

module WorkspaceConfiguration : sig
  type t = private (* interface *) Ojs.t

  type configuration_target =
    ([ `ConfigurationTarget of ConfigurationTarget.t
     | `Bool of bool
     ]
    [@js.union])

  [@@@js.implem
  let configuration_target_of_js js_val =
    match Ojs.type_of js_val with
    | "bool" -> `Bool (Ojs.bool_of_js js_val)
    | _      -> `ConfigurationTarget (ConfigurationTarget.t_of_js js_val)]

  type inspect_result =
    { key : string
    ; default_value : Jsonoo.t or_undefined
    ; global_value : Jsonoo.t or_undefined
    ; workspace_value : Jsonoo.t or_undefined
    ; workspace_folder_value : Jsonoo.t or_undefined
    ; default_language_value : Jsonoo.t or_undefined
    ; global_language_value : Jsonoo.t or_undefined
    ; workspace_language_value : Jsonoo.t or_undefined
    ; workspace_folder_language_value : Jsonoo.t or_undefined
    ; language_ids : string list or_undefined
    }

  val get : t -> section:string -> unit -> Jsonoo.t or_undefined [@@js.call]

  val has : t -> section:string -> bool [@@js.call]

  val inspect : t -> section:string -> inspect_result [@@js.call]

  val update :
       t
    -> section:string
    -> value:Jsonoo.t
    -> ?configuration_target:configuration_target
    -> ?override_in_language:bool
    -> unit
    -> Promise.void
    [@@js.call]
end

module StatusBarAlignment : sig
  type t =
    | Left [@js 1]
    | Right [@js 2]
  [@@js.enum]
end

module AccessibilityInformation : sig
  type t = private (* interface*) Ojs.t

  val label : t -> string [@@js.get]

  val role : t -> string or_undefined [@@js.get]

  val create : label:string -> ?role:string -> unit -> t [@@js.builder]
end

module StatusBarItem : sig
  type t = private (* interface *) Ojs.t

  type color =
    ([ `String of string
     | `ThemeColor of ThemeColor.t
     ]
    [@js.union])

  [@@@js.implem
  let color_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `ThemeColor (ThemeColor.t_of_js js_val)]

  type command =
    ([ `String of string
     | `Command of Command.t
     ]
    [@js.union])

  [@@@js.implem
  let command_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `Command (Command.t_of_js js_val)]

  val alignment : t -> StatusBarAlignment.t [@@js.get]

  val priority : t -> int or_undefined [@@js.get]

  val text : t -> string [@@js.get]

  val tooltip : t -> string or_undefined [@@js.get]

  val color : t -> color or_undefined

  val command : t -> command or_undefined

  val accessibility_information : t -> AccessibilityInformation.t or_undefined
    [@@js.get]

  val show : t -> unit [@@js.call]

  val hide : t -> unit [@@js.call]

  val dispose : t -> unit [@@js.call]

  val disposable : t -> Disposable.t
    [@@js.custom
      let disposable this = Disposable.make ~dispose:(fun () -> dispose this)]
end

module WorkspaceFoldersChangeEvent : sig
  type t = private (* interface *) Ojs.t

  val added : t -> WorkspaceFolder.t list [@@js.get]

  val removed : t -> WorkspaceFolder.t list [@@js.get]

  val create :
    added:WorkspaceFolder.t list -> removed:WorkspaceFolder.t list -> t
    [@@js.builder]
end

module FormattingOptions : sig
  type t = private (* interface *) Ojs.t

  val tab_size : t -> int [@@js.get]

  val insert_spaces : t -> bool [@@js.get]

  val create : tab_size:int -> insert_spaces:bool -> t [@@js.builder]
end

module Event : sig
  [@@@js.stop]

  type 'a t = listener:('a -> unit) -> Disposable.t

  [@@@js.start]

  [@@@js.implem
  type 'a t = listener:('a -> unit) -> Disposable.t

  let t_of_js ml_of_js js_fun =
   fun [@js.dummy] ~listener:ml_listener ->
    let js_listener =
      Ojs.fun_to_js 1 @@ fun js_arg -> ml_listener (ml_of_js js_arg)
    in
    let (disposable : Ojs.t) =
      Ojs.call js_fun "call" [| Ojs.null; js_listener |]
    in
    Disposable.t_of_js disposable

  let t_to_js ml_to_js ml_fun =
    Ojs.fun_to_js 1 @@ fun js_listener ->
    let ml_listener ml_arg =
      ignore @@ Ojs.call js_listener "call" [| Ojs.null; ml_to_js ml_arg |]
    in
    let (disposable : Disposable.t) = ml_fun ~listener:ml_listener in
    Disposable.t_to_js disposable]
end

module CancellationToken : sig
  type t = private (* interface *) Ojs.t

  val is_cancellation_requested : t -> bool [@@js.get]

  val on_cancellation_requested : t -> Ojs.t Event.t [@@js.get]

  val create :
       is_cancellation_requested:bool
    -> on_cancellation_requested:Ojs.t Event.t
    -> t
    [@@js.builder]
end

module QuickPickItem : sig
  type t = private (* interface *) Ojs.t

  val label : t -> string [@@js.get]

  val description : t -> string or_undefined [@@js.get]

  val detail : t -> string or_undefined [@@js.get]

  val picked : t -> bool or_undefined [@@js.get]

  val always_show : t -> bool or_undefined [@@js.get]

  val create :
       label:string
    -> ?description:string
    -> ?detail:string
    -> ?picked:bool
    -> ?always_show:bool
    -> unit
    -> t
    [@@js.builder]
end

module QuickPickOptions : sig
  type t = private (* interface *) Ojs.t

  type on_did_select_item_args =
    ([ `QuickPickItem of QuickPickItem.t
     | `String of string
     ]
    [@js.union])

  [@@@js.implem
  let on_did_select_item_args_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `QuickPickItem (QuickPickItem.t_of_js js_val)]

  val match_on_description : t -> bool or_undefined [@@js.get]

  val match_on_detail : t -> bool or_undefined [@@js.get]

  val place_holder : t -> string or_undefined [@@js.get]

  val ignore_focus_out : t -> bool or_undefined [@@js.get]

  val can_pick_many : t -> bool or_undefined [@@js.get]

  val on_did_select_item : t -> (on_did_select_item_args -> unit) or_undefined
    [@@js.get]

  val create :
       match_on_description:bool
    -> ?match_on_detail:bool
    -> ?place_holder:string
    -> ?ignore_focus_out:bool
    -> ?can_pick_many:bool
    -> ?on_did_select_item:(on_did_select_item_args -> unit)
    -> unit
    -> t
    [@@js.builder]
end

module ProviderResult : sig
  [@@@js.stop]

  type 'a t =
    [ `Value of 'a or_undefined
    | `Promise of 'a or_undefined Promise.t
    ]

  [@@@js.start]

  [@@@js.implem
  type 'a t =
    [ `Value of 'a or_undefined
    | `Promise of 'a or_undefined Promise.t
    ]

  let t_to_js to_js = function
    | `Value v   -> or_undefined_to_js to_js v
    | `Promise p -> Promise.t_to_js (or_undefined_to_js to_js) p

  let t_of_js of_js js_val =
    if Ojs.has_property js_val "then" then
      `Promise (Promise.t_of_js (or_undefined_of_js of_js) js_val)
    else
      `Value (or_undefined_of_js of_js js_val)]
end

module InputBoxOptions : sig
  type t = private (* interface *) Ojs.t

  val value : t -> string or_undefined [@@js.get]

  val value_selection : t -> (int * int) or_undefined [@@js.get]

  val prompt : t -> string or_undefined [@@js.get]

  val place_holder : t -> string or_undefined [@@js.get]

  val password : t -> bool or_undefined [@@js.get]

  val ignore_focus_out : t -> bool or_undefined [@@js.get]

  val validate_input : t -> (string -> string ProviderResult.t) or_undefined
    [@@js.get]

  val create :
       ?value:string
    -> ?value_selection:int * int
    -> ?prompt:string
    -> ?place_holder:string
    -> ?password:bool
    -> ?ignore_focus_out:bool
    -> ?validate_input:(string -> string option Promise.t)
    -> unit
    -> t
    [@@js.builder]
end

module MessageItem : sig
  type t = private (* interface *) Ojs.t

  val title : t -> string [@@js.get]

  val is_close_affordance : t -> bool or_undefined [@@js.get]

  val create : title:string -> ?is_close_affordance:bool -> unit -> t
    [@@js.builder]
end

module Location : sig
  type t = private (* class *) Ojs.t

  val uri : t -> Uri.t [@@js.get]

  val range : t -> Range.t [@@js.get]

  val make :
       uri:Uri.t
    -> range_or_position:([ `Range of Range.t | `Position of Position.t ]
         [@js.union])
    -> t
    [@@js.new "vscode.Location"]
end

module ProgressLocation : sig
  type t =
    | Source_control [@js 1]
    | Window [@js 10]
    | Notification [@js 25]
  [@@js.enum]
end

module ProgressOptions : sig
  type t = private (* interface *) Ojs.t

  module Location : sig
    type view_id_location = private (* interface *) Ojs.t

    type t =
      ([ `ProgressLocation of ProgressLocation.t
       | `ViewIdLocation of view_id_location
       ]
      [@js.union])

    [@@@js.implem
    let t_of_js js_val =
      match Ojs.type_of js_val with
      | "number" -> `ProgressLocation (ProgressLocation.t_of_js js_val)
      | _        -> `ViewIdLocation (view_id_location_of_js js_val)]

    val view_id : view_id_location -> string [@@js.get]

    val view_id_location : view_id:string -> view_id_location [@@js.builder]
  end

  val location : t -> Location.t [@@js.get]

  val title : t -> string or_undefined [@@js.get]

  val cancellable : t -> bool or_undefined [@@js.get]

  val create :
    location:Location.t -> ?title:string -> ?cancellable:bool -> unit -> t
    [@@js.builder]
end

module TextDocumentShowOptions : sig
  type t = private (* interface *) Ojs.t

  val view_column : t -> ViewColumn.t or_undefined [@@js.get]

  val preserve_focus : t -> bool or_undefined [@@js.get]

  val preview : t -> bool or_undefined [@@js.get]

  val selection : t -> Range.t or_undefined [@@js.get]

  val create :
       view_column:ViewColumn.t
    -> ?preserve_focus:bool
    -> ?preview:bool
    -> ?selection:Range.t
    -> unit
    -> t
    [@@js.builder]
end

module TerminalOptions : sig
  type t = private (* interface *) Ojs.t

  type shell_args =
    ([ `Arg of string
     | `Args of string list
     ]
    [@js.union])

  [@@@js.implem
  let shell_args_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `Arg (Ojs.string_of_js js_val)
    | _        -> `Args (Ojs.list_of_js Ojs.string_of_js js_val)]

  type cwd =
    ([ `String of string
     | `Uri of Uri.t
     ]
    [@js.union])

  [@@@js.implem
  let cwd_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `Uri (Uri.t_of_js js_val)]

  val name : t -> string or_undefined [@@js.get]

  val shell_path : t -> string or_undefined [@@js.get]

  val shell_args : t -> shell_args or_undefined [@@js.get]

  val cwd : t -> cwd or_undefined [@@js.get]

  val env : t -> string or_undefined Dict.t or_undefined [@@js.get]

  val strict_env : t -> bool [@@js.get]

  val hide_from_user : t -> bool [@@js.get]
end

module TerminalDimensions : sig
  type t = private (* interface *) Ojs.t

  val columns : t -> int [@@js.get]

  val rows : t -> int [@@js.get]

  val create : columns:int -> rows:int -> t [@@js.builder]
end

module Pseudoterminal : sig
  type t = private (* interface *) Ojs.t

  val on_did_write : t -> string Event.t [@@js.get]

  val on_did_override_dimensions :
    t -> TerminalDimensions.t or_undefined Event.t or_undefined
    [@@js.get]

  val on_did_close : t -> int or_undefined Event.t or_undefined [@@js.get]

  val open_ : t -> ?initial_dimensions:TerminalDimensions.t -> unit -> unit
    [@@js.call]

  val close : t -> unit [@@js.call]

  val handle_input : t -> (data:string -> unit) or_undefined [@@js.get]

  val set_dimensions :
    t -> (dimensions:TerminalDimensions.t -> unit) or_undefined
    [@@js.get]

  val create :
       on_did_write:string Event.t
    -> ?on_did_override_dimensions:TerminalDimensions.t or_undefined Event.t
    -> ?on_did_close:int or_undefined Event.t
    -> open_:(?initial_dimensions:TerminalDimensions.t -> unit -> unit)
    -> close:(unit -> unit)
    -> ?handle_input:(data:string -> unit)
    -> ?set_dimensions:(dimensions:TerminalDimensions.t -> unit)
    -> unit
    -> t
    [@@js.builder]
end

module ExtensionTerminalOptions : sig
  type t = private (* interface *) Ojs.t

  val name : t -> string [@@js.get]

  val pty : t -> Pseudoterminal.t [@@js.get]

  val create : name:string -> pty:Pseudoterminal.t -> t [@@js.builder]
end

module TerminalExitStatus : sig
  type t = private (* interface *) Ojs.t

  val code : t -> int [@@js.get]

  val create : code:int -> t [@@js.builder]
end

module Terminal : sig
  type t = private (* interface *) Ojs.t

  type creation_options =
    ([ `TerminalOptions of TerminalOptions.t
     | `ExtensionTerminalOptions of ExtensionTerminalOptions.t
     ]
    [@js.union])

  [@@@js.implem
  let creation_options_of_js js_val =
    if Ojs.has_property js_val "pty" then
      `ExtensionTerminalOptions (ExtensionTerminalOptions.t_of_js js_val)
    else
      `TerminalOptions (TerminalOptions.t_of_js js_val)]

  val name : t -> string [@@js.get]

  val process_id : t -> int or_undefined Promise.t [@@js.get]

  val creation_options : t -> creation_options [@@js.get]

  val exit_status : t -> TerminalExitStatus.t or_undefined [@@js.get]

  val send_text : t -> text:string -> ?add_new_line:bool -> unit -> unit
    [@@js.call]

  val show : t -> ?preserve_focus:bool -> unit -> unit [@@js.call]

  val hide : t -> unit [@@js.call]

  val dispose : t -> unit [@@js.call]

  val disposable : t -> Disposable.t
    [@@js.custom
      let disposable this = Disposable.make ~dispose:(fun () -> dispose this)]
end

module OutputChannel : sig
  type t = private (* interface *) Ojs.t

  val name : t -> string [@@js.get]

  val append : t -> value:string -> unit [@@js.call]

  val append_line : t -> value:string -> unit [@@js.call]

  val clear : t -> unit

  val show : t -> ?preserve_focus:bool -> unit -> unit [@@js.call]

  val hide : t -> unit [@@js.call]

  val dispose : t -> unit [@@js.call]

  val disposable : t -> Disposable.t
    [@@js.custom
      let disposable this = Disposable.make ~dispose:(fun () -> dispose this)]
end

module Memento : sig
  type t = private (* interface *) Ojs.t

  val get : t -> key:string -> Jsonoo.t or_undefined

  val update : t -> key:string -> value:Jsonoo.t -> Promise.void
end

module EnvironmentVariableMutatorType : sig
  type t =
    | Replace [@js 1]
    | Append [@js 2]
    | Prepend [@js 3]
  [@@js.enum]
end

module EnvironmentVariableMutator : sig
  type t = private (* interface *) Ojs.t

  val type_ : t -> EnvironmentVariableMutatorType.t [@@js.get]

  val value : t -> string [@@js.get]
end

module EnvironmentVariableCollection : sig
  type t = private (* interface *) Ojs.t

  val persistent : t -> bool [@@js.get]

  val replace : t -> variable:string -> value:string -> unit [@@js.call]

  val append : t -> variable:string -> value:string -> unit [@@js.call]

  val prepend : t -> variable:string -> value:string -> unit [@@js.call]

  val get : t -> variable:string -> EnvironmentVariableMutator.t or_undefined
    [@@js.call]

  val for_each :
       t
    -> callback:
         (   variable:string
          -> mutator:EnvironmentVariableMutator.t
          -> collection:t
          -> unit)
    -> unit
    [@@js.call]

  val delete : t -> variable:string -> unit [@@js.call]

  val clear : t -> unit [@@js.call]
end

module ExtensionMode : sig
  type t =
    | Production [@js 1]
    | Development [@js 2]
    | Test [@js 3]
  [@@js.enum]
end

module ExtensionContext : sig
  type t = private (* interface *) Ojs.t

  val subscriptions : t -> Disposable.t list [@@js.get]

  val workspace_state : t -> Memento.t [@@js.get]

  val global_state : t -> Memento.t [@@js.get]

  val extension_uri : t -> Uri.t [@@js.get]

  val extension_path : t -> string [@@js.get]

  val environment_variable_collection : t -> EnvironmentVariableCollection.t
    [@@js.get]

  val as_absolute_path : t -> relative_path:string -> string [@@js.call]

  val storage_path : t -> string or_undefined [@@js.get]

  val global_storage_path : t -> string [@@js.get]

  val log_path : t -> string [@@js.get]

  val extension_mode : t -> ExtensionMode.t [@@js.get]

  val subscribe : t -> disposable:Disposable.t -> unit
    [@@js.custom
      let subscribe this ~disposable =
        let subscriptions = Ojs.get this "subscriptions" in
        let (_ : Ojs.t) = Ojs.call subscriptions "push" [| disposable |] in
        ()]
end

module ShellQuotingOptions : sig
  type t = private (* interface *) Ojs.t

  module Escape : sig
    type literal = private (* interface *) Ojs.t

    type t =
      ([ `String of string
       | `Literal of literal
       ]
      [@js.union])

    [@@@js.implem
    let t_of_js js_val =
      match Ojs.type_of js_val with
      | "string" -> `String (Ojs.string_of_js js_val)
      | _        -> `Literal (t_of_js js_val)]

    val escape_char : literal -> string [@@js.get]

    val chars_to_escape : literal -> string [@@js.get]

    val literal : escape_char:string -> chars_to_escape:string -> literal
      [@@js.builder]
  end

  val escape : t -> Escape.t or_undefined [@@js.get]

  val strong : t -> string or_undefined [@@js.get]

  val weak : t -> string or_undefined [@@js.get]

  val create : ?escape:Escape.t -> ?strong:string -> ?weak:string -> unit -> t
    [@@js.builder]
end

module ShellExecutionOptions : sig
  type t = private (* interface *) Ojs.t

  val executable : t -> string or_undefined [@@js.get]

  val shell_args : t -> string list or_undefined [@@js.get]

  val shell_quoting : t -> ShellQuotingOptions.t or_undefined [@@js.get]

  val cwd : t -> string or_undefined [@@js.get]

  val env : t -> string Dict.t or_undefined [@@js.get]

  val create :
       ?executable:string
    -> ?shell_args:string list
    -> ?shell_quoting:ShellQuotingOptions.t
    -> ?cwd:string
    -> ?env:Ojs.t
    -> unit
    -> t
    [@@js.builder]
end

module ShellQuoting : sig
  type t =
    | Escape [@js 1]
    | Strong [@js 2]
    | Weak [@js 3]
  [@@js.enum]
end

module ShellQuotedString : sig
  type t = private (* interface *) Ojs.t

  val value : t -> string [@@js.get]

  val quoting : t -> ShellQuoting.t [@@js.get]

  val create : value:string -> quoting:ShellQuoting.t -> t [@@js.builder]
end

module ShellExecution : sig
  type t = private (* class *) Ojs.t

  type shell_string =
    ([ `String of string
     | `ShellQuotedString of ShellQuotedString.t
     ]
    [@js.union])

  [@@@js.implem
  let shell_string_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `ShellQuotedString (ShellQuotedString.t_of_js js_val)]

  val make_command_line :
    command_line:string -> ?options:ShellExecutionOptions.t -> unit -> t
    [@@js.new "vscode.ShellExecution"]

  val make_command_args :
       command:shell_string
    -> args:shell_string list
    -> ?options:ShellExecutionOptions.t
    -> unit
    -> t
    [@@js.new "vscode.ShellExecution"]

  val command_line : t -> string or_undefined [@@js.get]

  val command : t -> shell_string [@@js.get]

  val args : t -> shell_string list [@@js.get]

  val options : t -> ShellExecutionOptions.t or_undefined [@@js.get]
end

module ProcessExecutionOptions : sig
  type t = private (* interface *) Ojs.t

  val cwd : t -> string or_undefined [@@js.get]

  val env : t -> string Dict.t or_undefined [@@js.get]

  val create : ?cwd:string -> ?env:string Dict.t -> unit -> t [@@js.builder]
end

module ProcessExecution : sig
  type t = private (* class *) Ojs.t

  val make_process :
    process:string -> ?options:ProcessExecutionOptions.t -> unit -> t
    [@@js.new "vscode.ProcessExecution"]

  val make_process_args :
       process:string
    -> args:string list
    -> ?options:ProcessExecutionOptions.t
    -> unit
    -> t
    [@@js.new "vscode.ProcessExecution"]

  val process : t -> string [@@js.get]

  val args : t -> string list [@@js.get]

  val options : t -> ProcessExecutionOptions.t or_undefined [@@js.get]
end

module CustomExecution : sig
  type t = private (* class *) Ojs.t

  val make : callback:(unit -> Pseudoterminal.t Promise.t) -> t
    [@@js.new "vscode.CustomExecution"]
end

module RelativePattern : sig
  type t = private (* class *) Ojs.t

  val base : t -> string [@@js.get]

  val pattern : t -> string [@@js.get]

  val make :
       base:([ `WorkspaceFolder of WorkspaceFolder.t | `String of string ]
         [@js.union])
    -> pattern:string
    -> t
    [@@js.new "vscode.RelativePattern"]
end

module GlobPattern : sig
  type t =
    ([ `String of string
     | `RelativePattern of RelativePattern.t
     ]
    [@js.union])

  [@@@js.implem
  let t_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `RelativePattern (RelativePattern.t_of_js js_val)]
end

module DocumentFilter : sig
  type t = private (* interface *) Ojs.t

  val language : t -> string or_undefined [@@js.get]

  val scheme : t -> string or_undefined [@@js.get]

  val pattern : t -> GlobPattern.t or_undefined [@@js.get]

  val create :
    ?language:string -> ?scheme:string -> ?pattern:GlobPattern.t -> unit -> t
    [@@js.builder]
end

module DocumentSelector : sig
  type selectors =
    ([ `DocumentFilter of DocumentFilter.t
     | `String of string
     ]
    [@js.union])

  [@@@js.implem
  let selectors_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `DocumentFilter (DocumentFilter.t_of_js js_val)]

  type t =
    ([ `DocumentFilter of DocumentFilter.t
     | `String of string
     | `List of selectors list
     ]
    [@js.union])

  [@@@js.implem
  let t_of_js js_val =
    if Ojs.type_of js_val = "string" then
      `String (Ojs.string_of_js js_val)
    else if Ojs.has_property js_val "length" then
      `List (Ojs.list_of_js selectors_of_js js_val)
    else
      `DocumentFilter (DocumentFilter.t_of_js js_val)]

  val x : t -> t [@@js.get]
end

module DocumentFormattingEditProvider : sig
  type t = private (* interface *) Ojs.t

  val provide_document_formatting_edits :
       t
    -> document:TextDocument.t
    -> options:FormattingOptions.t
    -> token:CancellationToken.t
    -> TextEdit.t list ProviderResult.t
    [@@js.call]

  val create :
       provide_document_formatting_edits:
         (   t
          -> document:TextDocument.t
          -> options:FormattingOptions.t
          -> token:CancellationToken.t
          -> TextEdit.t list ProviderResult.t)
    -> t
    [@@js.builder]
end

module TaskGroup : sig
  type t = private (* class *) Ojs.t

  val clean : t [@@js.global "vscode.TaskGroup.Clean"]

  val build : t [@@js.global "vscode.TaskGroup.Build"]

  val rebuild : t [@@js.global "vscode.TaskGroup.Rebuild"]

  val test : t [@@js.global "vscode.TaskGroup.Test"]
end

module TaskScope : sig
  type t =
    | Global [@js 1]
    | Workspace [@js 2]
  [@@js.enum]
end

module RunOptions : sig
  type t = private (* interface *) Ojs.t

  val reevaluate_on_rerun : t -> bool or_undefined [@@js.get]

  val create : ?reevaluate_on_rerun:bool -> unit -> t [@@js.builder]
end

module TaskDefinition : sig
  type t = private (* interface *) Ojs.t

  val type_ : t -> string [@@js.get]

  val get_attribute : t -> string -> Ojs.t
    [@@js.custom let get_attribute = Ojs.get]

  val set_attribute : t -> string -> Ojs.t -> unit
    [@@js.custom let set_attribute = Ojs.set]

  val create : type_:string -> attributes:(string, Ojs.t) Hashtbl.t -> unit -> t
    [@@js.custom
      let create ~type_ ~attributes () =
        let obj = Ojs.obj [| ("type", Ojs.string_to_js type_) |] in
        Hashtbl.iter (Ojs.set obj) attributes;
        obj]
end

module TaskRevealKind : sig
  type t =
    | Always [@js 1]
    | Silent [@js 2]
    | Never [@js 3]
  [@@js.enum]
end

module TaskPanelKind : sig
  type t =
    | Shared [@js 1]
    | Dedicated [@js 2]
    | New [@js 3]
  [@@js.enum]
end

module TaskPresentationOptions : sig
  type t = private (* interface *) Ojs.t

  val reveal : t -> TaskRevealKind.t or_undefined [@@js.get]

  val echo : t -> bool or_undefined [@@js.get]

  val focus : t -> bool or_undefined [@@js.get]

  val panel : t -> TaskPanelKind.t or_undefined [@@js.get]

  val show_reuse_message : t -> bool or_undefined [@@js.get]

  val clear : t -> bool or_undefined [@@js.get]

  val create :
       ?reveal:TaskRevealKind.t
    -> ?echo:bool
    -> ?focus:bool
    -> ?panel:TaskPanelKind.t
    -> ?show_reuse_message:bool
    -> ?clear:bool
    -> unit
    -> t
    [@@js.builder]
end

module Task : sig
  type t = private (* class *) Ojs.t

  type scope =
    ([ `WorkspaceFolder of WorkspaceFolder.t
     | `TaskScope of TaskScope.t
     ]
    [@js.union])

  [@@@js.implem
  let scope_of_js js_val =
    match Ojs.type_of js_val with
    | "number" -> `TaskScope (TaskScope.t_of_js js_val)
    | _        -> `WorkspaceFolder (WorkspaceFolder.t_of_js js_val)]

  type execution =
    ([ `ProcessExecution of ProcessExecution.t
     | `ShellExecution of ShellExecution.t
     | `CustomExecution of CustomExecution.t
     ]
    [@js.union])

  [@@@js.implem
  let execution_of_js js_val =
    if Ojs.has_property js_val "process" then
      `ProcessExecution (ProcessExecution.t_of_js js_val)
    else if Ojs.has_property js_val "command" then
      `ShellExecution (ShellExecution.t_of_js js_val)
    else
      `CustomExecution (CustomExecution.t_of_js js_val)]

  type problem_matchers =
    ([ `String of string
     | `Strings of string list
     ]
    [@js.union])

  [@@@js.implem
  let problem_matchers_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `Strings (Ojs.list_of_js Ojs.string_of_js js_val)]

  val make :
       definition:TaskDefinition.t
    -> scope:scope
    -> name:string
    -> source:string
    -> ?execution:execution
    -> ?problem_matchers:problem_matchers
    -> unit
    -> t
    [@@js.new "vscode.Task"]

  val definition : t -> TaskDefinition.t [@@js.get]

  val scope : t -> scope or_undefined [@@js.get]

  val name : t -> string [@@js.get]

  val execution : t -> execution or_undefined [@@js.get]

  val is_background : t -> bool [@@js.get]

  val source : t -> string [@@js.get]

  val group : t -> TaskGroup.t or_undefined [@@js.get]

  val presentation_options : t -> TaskPresentationOptions.t [@@js.get]

  val run_options : t -> RunOptions.t [@@js.get]
end

module TaskProvider : sig
  type t = private (* interface *) Ojs.t

  val provide_tasks :
    t -> ?token:CancellationToken.t -> unit -> Task.t list ProviderResult.t
    [@@js.call]

  val resolve_tasks :
       t
    -> task:Task.t
    -> ?token:CancellationToken.t
    -> unit
    -> Task.t ProviderResult.t
    [@@js.call]

  val create :
       provide_tasks:
         (?token:CancellationToken.t -> unit -> Task.t list ProviderResult.t)
    -> resolve_tasks:
         (   task:Task.t
          -> ?token:CancellationToken.t
          -> unit
          -> Task.t ProviderResult.t)
    -> t
    [@@js.builder]
end
