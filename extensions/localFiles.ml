(* Auxiliary functions to display a local file or directory. Currently
used in staticmod and eliom_predefmod*)

exception Failed_403
exception Failed_404


type options = {
  list_directory_content : bool;
  follow_symlinks: follow_symlink
}
and follow_symlink =
  | DoNotFollow (* Never follow a symlink *)
  | FollowIfOwnerMatch (* Follow a symlink if the symlink and its
                          target have the same owner *)
  | AlwaysFollow (* Always follow symlinks *)



(* checks that [filename] can be followed depending on its third argument.
   [stat] must be the result of calling [Unix.stat] on filename *)
let should_follow filename stat = function
  | AlwaysFollow -> true
  | f ->
      let lstat = Unix.LargeFile.lstat filename in
      if lstat.Unix.LargeFile.st_kind = Unix.S_LNK then
        if f = DoNotFollow then
          false
        else
          stat.Unix.LargeFile.st_uid = lstat.Unix.LargeFile.st_uid
      else
        true

(* Return type of a request for a local file. The string argument
   represents the real file/directory to serve, eg. foo/index.html
   instead of foo *)
type resolved =
  | RFile of string
  | RDir of string


(* given [filename], we search for it in the local filesystem and
   - we return ["filename/index.html"] if [filename] corresponds to
   a directory, and ["filename/index.html"] is valid
   - we raise [Failed_403] if [filename] corresponds to a directory,
   ["filename/index.html"] does not exists and [list_dir_content] is false
   - we raise [Failed_403] if [filename] is a symlink that must
   not be followed
   - raises [Failed_404] if [filename] does not exist, or is a special file
   - otherwise returns [filename]
*)
(* See also module Files in eliom.ml *)
let resolve ~filename ~options =
  try
    Ocsigen_messages.debug
      (fun () -> "--Resolve_local_file: Testing \""^filename^"\".");
    let stat = Unix.LargeFile.stat filename in
    let (filename, stat) =
      if stat.Unix.LargeFile.st_kind = Unix.S_DIR then
        if filename.[String.length filename - 1] <> '/' then begin
          (* In this case, [filename] is a directory but this is not visible in
             its name as there is no final slash. We signal this fact to
             Ocsigen, which will then issue a 301 redirection to "filename/" *)
          Ocsigen_messages.debug
            (fun () -> "--Resolve_local_file: "^filename^" is a directory");
          raise Ocsigen_extensions.Ocsigen_Is_a_directory

        end else begin
          let fn2 = filename ^ "index.html" in
          Ocsigen_messages.debug
            (fun () -> "--Resolve_local_file: Testing \""^fn2^"\".");
          try
            (fn2, Unix.LargeFile.stat fn2)
          with
            | Unix.Unix_error (Unix.ENOENT, _, _) ->
                if options.list_directory_content then
                  (filename, stat)
                else
                  raise Failed_403
        end
      else (filename, stat)
    in
    if should_follow filename stat options.follow_symlinks then
      begin
        Ocsigen_messages.debug
          (fun () -> "--Resolve_local_file: Looking for \""^filename^"\".");
        if stat.Unix.LargeFile.st_kind = Unix.S_REG then
          RFile filename
        else if stat.Unix.LargeFile.st_kind = Unix.S_DIR then
          RDir filename
        else raise Failed_404
      end
    else
      (* [filename] is a symlink which we should not follow according
         to the current policy *)
      raise Failed_403
  with
    | Unix.Unix_error (Unix.ENOENT,_,_) -> raise Failed_404


(* Given a local file or directory, we retrieve its content *)
let content ~url ~file =
  match file with
    | RDir dirname ->
        Ocsigen_senders.Directory_content.result_of_content (dirname, url)
    | RFile filename ->
        Ocsigen_senders.File_content.result_of_content filename
