let name_max = 255
let path_max = 4096

type mkdir_result =
  | Created
  | Already_exists
  | Parent_dir_missing

type mkdir_p_result =
  | Already_exists
  | Created

let mkdir ?(perm = 0o777) name : mkdir_result = 
  try
    Unix.mkdir name perm;
    Created
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> Already_exists
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Parent_dir_missing
;;

let rec mkdir_p ?perm path : mkdir_p_result =
  match mkdir ?perm path with
  | Created -> Created
  | Already_exists -> Already_exists
  | Parent_dir_missing ->
      let parent_dir = Filename.dirname path in
      match mkdir_p ?perm parent_dir with
      | Created | Already_exists ->
          (match mkdir ?perm path with
           | Created -> Created
           | Already_exists -> Already_exists
           | Parent_dir_missing ->
               raise (Failure ("Failed to create parent component of " ^ path)))
;;
