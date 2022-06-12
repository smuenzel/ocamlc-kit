
module type S = sig
  module Arch : Arch_intf.S
  module CSE : CSE_intf.S
    with type addressing_mode = Arch.addressing_mode
     and type specific_operation = Arch.specific_operation
  module Emit : Emit_intf.S
    with type addressing_mode = Arch.addressing_mode
     and type specific_operation = Arch.specific_operation
  module Proc : Proc_intf.S
    with type addressing_mode = Arch.addressing_mode
     and type specific_operation = Arch.specific_operation
  module Scheduling : Scheduling_intf.S
    with type addressing_mode = Arch.addressing_mode
     and type specific_operation = Arch.specific_operation
  module Selection : Selection_intf.S
    with type addressing_mode = Arch.addressing_mode
     and type specific_operation = Arch.specific_operation
  module Reload : Reload_intf.S
    with type addressing_mode = Arch.addressing_mode
     and type specific_operation = Arch.specific_operation
end
