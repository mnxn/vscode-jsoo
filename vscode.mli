val get_version : unit -> string [@@js.get "vscode.version"]

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

         method translate : ?lineDelta:int -> ?characterDelta:int -> unit -> t

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
       startLine:int
    -> startCharacter:int
    -> endLine:int
    -> endCharacter:int
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
       title:string
    -> command:string
    -> ?tooltip:string
    -> ?arguments:Ojs.t array
    -> unit
    -> t
    [@@js.builder]
end [@scope "vscode"]
