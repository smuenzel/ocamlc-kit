
module type S = sig
  type addressing_mode
  type specific_operation

  val fundecl
    :  (addressing_mode, specific_operation) Mach.fundecl
    -> (addressing_mode, specific_operation) Mach.fundecl

end
