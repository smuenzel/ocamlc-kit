
type immediate_or_pointer =
  | Immediate
  | Pointer

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval

and boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64
