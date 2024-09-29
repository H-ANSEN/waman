type mkdir_result =
  | Created
  | Already_exists
  | Parent_dir_missing

type mkdir_p_result =
  | Already_exists
  | Created

(** The maximum length in bytes that a file name can have on unix systems. *)
val name_max : int

(** The maximum length in bytes that a directory path can have on unix
    systems. *)
val path_max : int

val mkdir : ?perm:int -> string -> mkdir_result
val mkdir_p : ?perm:int -> string -> mkdir_p_result
