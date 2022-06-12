open Format

module type S = sig
  val size_int : int
  val size_addr : int
  val size_float : int
  val big_endian : bool
  val division_crashes_on_overflow : bool
  val allow_unaligned_access : bool
  type addressing_mode [@@deriving sexp_of]
  type specific_operation [@@deriving sexp_of]
  val operation_is_pure : specific_operation -> bool
  val operation_can_raise : specific_operation -> bool

  val identity_addressing : addressing_mode

  val print_addressing : (formatter -> 'a -> unit) -> addressing_mode -> formatter -> 'a array -> unit
  val print_specific_operation : (formatter -> 'a -> unit) -> specific_operation -> formatter -> 'a array -> unit
  
  val offset_addressing : addressing_mode -> int -> addressing_mode

  val command_line_options : (string * Arg.spec * string) list

end
