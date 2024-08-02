open Instructions

type typ = I32 | I64 | F32 | F64

type func = {
  name : string;
  params : (string option * typ) list;
  results : typ list;
  locals : (string option * typ) list;
  body : expr;
}

type memory = { min_pages : int; max_pages : int option }
type import_desc = FuncImport of (string * typ list * typ list)
type import = { module_name : string; name : string; desc : import_desc }
type export_desc = FuncExport of string
type export = { name : string; desc : export_desc }
type data = { offset : int; value : string }

type module_ = {
  imports : import list;
  exports : export list;
  funcs : func list;
  memory : memory option;
  data : data list;
}

let string_of_typ typ =
  match typ with I32 -> "i32" | I64 -> "i64" | F32 -> "f32" | F64 -> "f64"

let string_of_params params =
  List.map
    (fun (name, typ) ->
      match name with
      | Some name -> Printf.sprintf "(param $%s %s)" name (string_of_typ typ)
      | None -> Printf.sprintf "(param %s)" (string_of_typ typ))
    params
  |> String.concat " "

let string_of_results results =
  List.map string_of_typ results |> String.concat " " |> fun s ->
  if s = "" then "" else Printf.sprintf "(result %s)" s

let string_of_locals locals =
  List.map
    (fun (name, typ) ->
      match name with
      | Some name -> Printf.sprintf "(local $%s %s)" name (string_of_typ typ)
      | None -> Printf.sprintf "(local %s)" (string_of_typ typ))
    locals
  |> String.concat " "

let string_of_func func =
  let params_str = string_of_params func.params in
  let results_str = string_of_results func.results in
  let locals_str = string_of_locals func.locals in
  let body_str = string_of_expr func.body in
  Printf.sprintf "(func $%s %s %s %s\n%s\n)" func.name params_str results_str
    locals_str body_str

let string_of_import_desc import_desc =
  match import_desc with
  | FuncImport (name, params, results) ->
      let params_str = List.map string_of_typ params |> String.concat " " in
      let results_str = List.map string_of_typ results |> String.concat " " in
      if results_str = "" then
        Printf.sprintf "(func $%s (param %s))" name params_str
      else
        Printf.sprintf "(func $%s (param %s) (result %s))" name params_str
          results_str

let string_of_import import =
  Printf.sprintf "(import \"%s\" \"%s\" %s)" import.module_name import.name
    (string_of_import_desc import.desc)

let string_of_export_desc export_desc =
  match export_desc with FuncExport name -> Printf.sprintf "(func $%s)" name

let string_of_export export =
  Printf.sprintf "(export \"%s\" %s)" export.name
    (string_of_export_desc export.desc)

let string_of_memory memory =
  match memory.max_pages with
  | Some max_pages ->
      Printf.sprintf "(memory (export \"memory\") %d %d)" memory.min_pages
        max_pages
  | None -> Printf.sprintf "(memory (export \"memory\") %d)" memory.min_pages

let string_of_data data =
  Printf.sprintf "(data (i32.const %d) \"%s\")" data.offset data.value

let string_of_module module_ =
  let imports_str =
    List.map string_of_import module_.imports |> String.concat "\n"
  in
  let exports_str =
    List.map string_of_export module_.exports |> String.concat "\n"
  in
  let funcs_str =
    List.map string_of_func module_.funcs |> String.concat "\n\n"
  in
  let data_str = List.map string_of_data module_.data |> String.concat "\n" in
  match module_.memory with
  | Some memory ->
      Printf.sprintf "(module\n%s\n%s\n%s\n%s\n%s\n)" imports_str
        (string_of_memory memory) funcs_str exports_str data_str
  | None ->
      Printf.sprintf "(module\n%s\n%s\n%s\n%s\n)" imports_str funcs_str
        exports_str data_str
