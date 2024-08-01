type value = Int32 of int | Int64 of int | Float32 of float | Float64 of float

let string_of_value v =
  match v with
  | Int32 n | Int64 n -> string_of_int n
  | Float32 f | Float64 f -> string_of_float f
