open Interop

module RevealOutputChannelOn : sig
  type t =
    | Info [@js 1]
    | Warn [@js 2]
    | Error [@js 3]
    | Never [@js 4]
  [@@js.enum]
end

module ServerCapabilities : sig
  type t = private (* interface *) Ojs.t

  val experimental : t -> Jsonoo.t or_undefined [@@js.get]

  val create : ?experimental:Jsonoo.t -> unit -> t [@@js.builder]
end

module InitializeResult : sig
  type server_info =
    { name : string
    ; version : string or_undefined
    }

  type t = private (* interface *) Ojs.t

  val capabilities : t -> ServerCapabilities.t [@@js.get]

  val server_info : t -> server_info or_undefined [@@js.get]
end

module DocumentFilter : sig
  type t = private (* interface *) Ojs.t

  val language : t -> string or_undefined [@@js.get]

  val scheme : t -> string or_undefined [@@js.get]

  val pattern : t -> string or_undefined [@@js.get]

  val create_language :
    language:string -> ?scheme:string -> ?pattern:string -> unit -> t
    [@@js.builder]

  val create_scheme :
    ?language:string -> scheme:string -> ?pattern:string -> unit -> t
    [@@js.builder]

  val create_pattern :
    ?language:string -> ?scheme:string -> pattern:string -> unit -> t
    [@@js.builder]
end

module DocumentSelector : sig
  type elem =
    ([ `Filter of DocumentFilter.t
     | `String of string
     ]
    [@js.union])

  [@@@js.implem
  let elem_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String (Ojs.string_of_js js_val)
    | _        -> `Filter (DocumentFilter.t_of_js js_val)]

  type t = elem array
end

module ClientOptions : sig
  type t = private (* interface *) Ojs.t

  val document_selector : t -> DocumentSelector.t or_undefined [@@js.get]

  val output_channel : t -> Vscode.OutputChannel.t or_undefined [@@js.get]

  val reveal_output_channel_on : t -> RevealOutputChannelOn.t [@@js.get]

  val create :
       ?document_selector:DocumentSelector.t
    -> ?output_channel:Vscode.OutputChannel.t
    -> ?reveal_output_channel_on:RevealOutputChannelOn.t
    -> unit
    -> t
    [@@js.builder]
end

module ExecutableOptions : sig
  type t = private (* interface *) Ojs.t

  val cwd : t -> string or_undefined [@@js.get]

  val env : t -> Jsonoo.t or_undefined [@@js.get]

  val detached : t -> bool or_undefined [@@js.get]

  val shell : t -> bool or_undefined [@@js.get]

  val create :
    ?cwd:string -> ?env:Jsonoo.t -> ?detached:bool -> ?shell:bool -> unit -> t
    [@@js.builder]
end

module Executable : sig
  type t = private (* interface *) Ojs.t

  val command : t -> string [@@js.get]

  val args : t -> string array or_undefined [@@js.get]

  val options : t -> ExecutableOptions.t or_undefined [@@js.get]

  val create :
       command:string
    -> ?args:string array
    -> ?options:ExecutableOptions.t
    -> unit
    -> t
    [@@js.builder]
end

module ServerOptions : sig
  type t = Executable.t (* TODO other union types *)
end

module LanguageClient : sig
  type t = private (* class *) Ojs.t

  val make :
       id:string
    -> name:string
    -> server_options:ServerOptions.t
    -> client_options:ClientOptions.t
    -> ?force_debug:bool
    -> unit
    -> t
    [@@js.new "LanguageClient"]

  val start : t -> unit [@@js.call]

  val stop : t -> unit [@@js.call]

  val on_ready : t -> Promise.void [@@js.call]

  val initialize_result : t -> InitializeResult.t or_undefined [@@js.get]
end
