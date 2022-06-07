val first_non_constant_constructor_tag : int
val last_non_constant_constructor_tag : int

val forcing_tag : int
val cont_tag : int
val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int   (* both [string] and [bytes] *)
val double_tag : int
val double_array_tag : int
val custom_tag : int

val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int   (* should never happen @since 3.11.0 *)
