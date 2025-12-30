(** Git operations wrapped with Eio and robust error handling. *)

let src = Logs.Src.create "unpac.git" ~doc:"Git operations"
module Log = (val Logs.src_log src : Logs.LOG)

(* Error types *)

type error =
  | Command_failed of {
      cmd : string list;
      exit_code : int;
      stdout : string;
      stderr : string;
    }
  | Not_a_repository
  | Remote_exists of string
  | Remote_not_found of string
  | Branch_exists of string
  | Branch_not_found of string
  | Merge_conflict of { branch : string; conflicting_files : string list }
  | Rebase_conflict of { onto : string; hint : string }
  | Uncommitted_changes
  | Not_on_branch
  | Detached_head

let pp_error fmt = function
  | Command_failed { cmd; exit_code; stderr; _ } ->
      Format.fprintf fmt "git %a failed (exit %d): %s"
        Fmt.(list ~sep:sp string) cmd exit_code
        (String.trim stderr)
  | Not_a_repository ->
      Format.fprintf fmt "not a git repository"
  | Remote_exists name ->
      Format.fprintf fmt "remote '%s' already exists" name
  | Remote_not_found name ->
      Format.fprintf fmt "remote '%s' not found" name
  | Branch_exists name ->
      Format.fprintf fmt "branch '%s' already exists" name
  | Branch_not_found name ->
      Format.fprintf fmt "branch '%s' not found" name
  | Merge_conflict { branch; conflicting_files } ->
      Format.fprintf fmt "merge conflict in '%s': %a" branch
        Fmt.(list ~sep:comma string) conflicting_files
  | Rebase_conflict { onto; hint } ->
      Format.fprintf fmt "rebase conflict onto '%s': %s" onto hint
  | Uncommitted_changes ->
      Format.fprintf fmt "uncommitted changes in working directory"
  | Not_on_branch ->
      Format.fprintf fmt "not on any branch"
  | Detached_head ->
      Format.fprintf fmt "HEAD is detached"

type Eio.Exn.err += E of error

let () =
  Eio.Exn.register_pp (fun fmt -> function
    | E e -> Format.fprintf fmt "Git %a" pp_error e; true
    | _ -> false)

let err e = Eio.Exn.create (E e)

(* Types *)

type proc_mgr = [ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t
type path = Eio.Fs.dir_ty Eio.Path.t

(* Helpers *)

let string_trim s = String.trim s

let lines s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.trim s <> "")

(* Low-level execution *)

let run ~proc_mgr ?cwd ?audit args =
  let full_cmd = "git" :: args in
  Log.debug (fun m -> m "Running: %a" Fmt.(list ~sep:sp string) full_cmd);
  let started = Unix.gettimeofday () in
  let cwd_str = match cwd with Some p -> snd p | None -> Sys.getcwd () in
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  try
    Eio.Switch.run @@ fun sw ->
    let stdout_r, stdout_w = Eio.Process.pipe proc_mgr ~sw in
    let stderr_r, stderr_w = Eio.Process.pipe proc_mgr ~sw in
    let child = Eio.Process.spawn proc_mgr ~sw
        ?cwd:(Option.map (fun p -> (p :> Eio.Fs.dir_ty Eio.Path.t)) cwd)
        ~stdout:stdout_w ~stderr:stderr_w
        full_cmd
    in
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;
    (* Read stdout and stderr concurrently *)
    Eio.Fiber.both
      (fun () ->
         let chunk = Cstruct.create 4096 in
         let rec loop () =
           match Eio.Flow.single_read stdout_r chunk with
           | n ->
               Buffer.add_string stdout_buf (Cstruct.to_string (Cstruct.sub chunk 0 n));
               loop ()
           | exception End_of_file -> ()
         in
         loop ())
      (fun () ->
         let chunk = Cstruct.create 4096 in
         let rec loop () =
           match Eio.Flow.single_read stderr_r chunk with
           | n ->
               Buffer.add_string stderr_buf (Cstruct.to_string (Cstruct.sub chunk 0 n));
               loop ()
           | exception End_of_file -> ()
         in
         loop ());
    let status = Eio.Process.await child in
    let stdout = Buffer.contents stdout_buf in
    let stderr = Buffer.contents stderr_buf in
    let exit_code, result = match status with
      | `Exited 0 ->
          Log.debug (fun m -> m "Output: %s" (string_trim stdout));
          0, Ok stdout
      | `Exited code ->
          Log.debug (fun m -> m "Failed (exit %d): %s" code (string_trim stderr));
          code, Error (Command_failed { cmd = args; exit_code = code; stdout; stderr })
      | `Signaled signal ->
          Log.debug (fun m -> m "Killed by signal %d" signal);
          let code = 128 + signal in
          code, Error (Command_failed { cmd = args; exit_code = code; stdout; stderr })
    in
    (* Record to audit if provided *)
    Option.iter (fun ctx ->
      let git_result : Audit.git_result = { exit_code; stdout; stderr } in
      Audit.record_git ctx ~cmd:args ~cwd:cwd_str ~started ~result:git_result
    ) audit;
    result
  with exn ->
    Log.err (fun m -> m "Exception running git: %a" Fmt.exn exn);
    raise exn

let run_exn ~proc_mgr ?cwd ?audit args =
  match run ~proc_mgr ?cwd ?audit args with
  | Ok output -> output
  | Error e ->
      let ex = err e in
      raise (Eio.Exn.add_context ex "running git %a" Fmt.(list ~sep:sp string) args)

let run_lines ~proc_mgr ?cwd ?audit args =
  run_exn ~proc_mgr ?cwd ?audit args |> string_trim |> lines

(* Queries *)

let is_repository path =
  let git_dir = Eio.Path.(path / ".git") in
  match Eio.Path.kind ~follow:false git_dir with
  | `Directory | `Regular_file -> true  (* .git can be a file for worktrees *)
  | _ -> false
  | exception _ -> false

let current_branch ~proc_mgr ~cwd =
  match run ~proc_mgr ~cwd ["symbolic-ref"; "--short"; "HEAD"] with
  | Ok output -> Some (string_trim output)
  | Error _ -> None

let current_branch_exn ~proc_mgr ~cwd =
  match current_branch ~proc_mgr ~cwd with
  | Some b -> b
  | None -> raise (err Not_on_branch)

let current_head ~proc_mgr ~cwd =
  run_exn ~proc_mgr ~cwd ["rev-parse"; "HEAD"] |> string_trim

let has_uncommitted_changes ~proc_mgr ~cwd =
  let status = run_exn ~proc_mgr ~cwd ["status"; "--porcelain"] in
  String.trim status <> ""

let remote_exists ~proc_mgr ~cwd name =
  match run ~proc_mgr ~cwd ["remote"; "get-url"; name] with
  | Ok _ -> true
  | Error _ -> false

let branch_exists ~proc_mgr ~cwd name =
  match run ~proc_mgr ~cwd ["show-ref"; "--verify"; "--quiet"; "refs/heads/" ^ name] with
  | Ok _ -> true
  | Error _ -> false

let rev_parse ~proc_mgr ~cwd ref_ =
  match run ~proc_mgr ~cwd ["rev-parse"; "--verify"; "--quiet"; ref_] with
  | Ok output -> Some (string_trim output)
  | Error _ -> None

let rev_parse_exn ~proc_mgr ~cwd ref_ =
  match rev_parse ~proc_mgr ~cwd ref_ with
  | Some sha -> sha
  | None -> raise (err (Branch_not_found ref_))

let rev_parse_short ~proc_mgr ~cwd ref_ =
  run_exn ~proc_mgr ~cwd ["rev-parse"; "--short"; ref_] |> string_trim

let ls_remote_default_branch ~proc_mgr ~cwd ~url =
  Log.info (fun m -> m "Detecting default branch for %s..." url);
  (* Try to get the default branch from the remote *)
  let output = run_exn ~proc_mgr ~cwd ["ls-remote"; "--symref"; url; "HEAD"] in
  (* Parse output like: ref: refs/heads/main\tHEAD *)
  let default =
    let lines = String.split_on_char '\n' output in
    List.find_map (fun line ->
      if String.starts_with ~prefix:"ref:" line then
        let parts = String.split_on_char '\t' line in
        match parts with
        | ref_part :: _ ->
            let ref_part = String.trim ref_part in
            if String.starts_with ~prefix:"ref: refs/heads/" ref_part then
              Some (String.sub ref_part 16 (String.length ref_part - 16))
            else None
        | _ -> None
      else None
    ) lines
  in
  match default with
  | Some branch ->
      Log.info (fun m -> m "Default branch: %s" branch);
      branch
  | None ->
      (* Fallback: try common branch names *)
      Log.debug (fun m -> m "Could not detect default branch, trying common names...");
      let try_branch name =
        match run ~proc_mgr ~cwd ["ls-remote"; "--heads"; url; name] with
        | Ok output when String.trim output <> "" -> true
        | _ -> false
      in
      if try_branch "main" then "main"
      else if try_branch "master" then "master"
      else begin
        Log.warn (fun m -> m "Could not detect default branch, assuming 'main'");
        "main"
      end

let list_remotes ~proc_mgr ~cwd =
  run_lines ~proc_mgr ~cwd ["remote"]

let remote_url ~proc_mgr ~cwd name =
  match run ~proc_mgr ~cwd ["remote"; "get-url"; name] with
  | Ok output -> Some (string_trim output)
  | Error _ -> None

let log_oneline ~proc_mgr ~cwd ?max_count from_ref to_ref =
  let range = from_ref ^ ".." ^ to_ref in
  let args = ["log"; "--oneline"; range] in
  let args = match max_count with
    | Some n -> args @ ["--max-count"; string_of_int n]
    | None -> args
  in
  run_lines ~proc_mgr ~cwd args

let diff_stat ~proc_mgr ~cwd from_ref to_ref =
  let range = from_ref ^ ".." ^ to_ref in
  run_exn ~proc_mgr ~cwd ["diff"; "--stat"; range]

let ls_tree ~proc_mgr ~cwd ~tree ~path =
  match run ~proc_mgr ~cwd ["ls-tree"; tree; path] with
  | Ok output -> String.trim output <> ""
  | Error _ -> false

let rev_list_count ~proc_mgr ~cwd from_ref to_ref =
  let range = from_ref ^ ".." ^ to_ref in
  let output = run_exn ~proc_mgr ~cwd ["rev-list"; "--count"; range] in
  int_of_string (string_trim output)

(* Idempotent mutations *)

let ensure_remote ~proc_mgr ~cwd ~name ~url =
  match remote_url ~proc_mgr ~cwd name with
  | None ->
      Log.info (fun m -> m "Adding remote %s -> %s" name url);
      run_exn ~proc_mgr ~cwd ["remote"; "add"; name; url] |> ignore;
      `Created
  | Some existing_url ->
      if existing_url = url then begin
        Log.debug (fun m -> m "Remote %s already exists with correct URL" name);
        `Existed
      end else begin
        Log.info (fun m -> m "Updating remote %s URL: %s -> %s" name existing_url url);
        run_exn ~proc_mgr ~cwd ["remote"; "set-url"; name; url] |> ignore;
        `Updated
      end

let ensure_branch ~proc_mgr ~cwd ~name ~start_point =
  if branch_exists ~proc_mgr ~cwd name then begin
    Log.debug (fun m -> m "Branch %s already exists" name);
    `Existed
  end else begin
    Log.info (fun m -> m "Creating branch %s at %s" name start_point);
    run_exn ~proc_mgr ~cwd ["branch"; name; start_point] |> ignore;
    `Created
  end

let ensure_vendored_remotes ~proc_mgr ~cwd (packages : Config.vendored_package list) =
  let created = ref 0 in
  List.iter (fun (pkg : Config.vendored_package) ->
    let remote_name = "origin-" ^ pkg.pkg_name in
    match ensure_remote ~proc_mgr ~cwd ~name:remote_name ~url:pkg.pkg_url with
    | `Created ->
        Log.info (fun m -> m "Recreated remote %s -> %s" remote_name pkg.pkg_url);
        incr created
    | `Updated ->
        Log.info (fun m -> m "Updated remote %s -> %s" remote_name pkg.pkg_url)
    | `Existed -> ()
  ) packages;
  !created

(* State-changing operations *)

let init ~proc_mgr ~cwd =
  Log.info (fun m -> m "Initializing git repository...");
  run_exn ~proc_mgr ~cwd ["init"] |> ignore

let fetch ~proc_mgr ~cwd ~remote =
  Log.info (fun m -> m "Fetching from %s..." remote);
  run_exn ~proc_mgr ~cwd ["fetch"; remote] |> ignore

let fetch_with_tags ~proc_mgr ~cwd ~remote =
  Log.info (fun m -> m "Fetching from %s (with tags)..." remote);
  run_exn ~proc_mgr ~cwd ["fetch"; "--tags"; "--force"; remote] |> ignore

let resolve_branch_or_tag ~proc_mgr ~cwd ~remote ~ref_name =
  (* Try as a remote tracking branch first *)
  let branch_ref = remote ^ "/" ^ ref_name in
  match rev_parse ~proc_mgr ~cwd branch_ref with
  | Some _ -> branch_ref
  | None ->
      (* Try as a tag *)
      let tag_ref = "refs/tags/" ^ ref_name in
      match rev_parse ~proc_mgr ~cwd tag_ref with
      | Some _ -> tag_ref
      | None ->
          failwith (Printf.sprintf "Ref not found: %s (tried branch %s and tag %s)"
                      ref_name branch_ref tag_ref)

let checkout ~proc_mgr ~cwd ref_ =
  Log.debug (fun m -> m "Checking out %s" ref_);
  run_exn ~proc_mgr ~cwd ["checkout"; ref_] |> ignore

let checkout_orphan ~proc_mgr ~cwd name =
  Log.info (fun m -> m "Creating orphan branch %s" name);
  run_exn ~proc_mgr ~cwd ["checkout"; "--orphan"; name] |> ignore

let read_tree_prefix ~proc_mgr ~cwd ~prefix ~tree =
  Log.debug (fun m -> m "Reading tree %s with prefix %s" tree prefix);
  run_exn ~proc_mgr ~cwd ["read-tree"; "--prefix=" ^ prefix; tree] |> ignore

let checkout_index ~proc_mgr ~cwd =
  Log.debug (fun m -> m "Checking out index to working directory");
  run_exn ~proc_mgr ~cwd ["checkout-index"; "-a"; "-f"] |> ignore

let rm_rf ~proc_mgr ~cwd ~target =
  Log.debug (fun m -> m "Removing %s from git" target);
  (* Ignore errors - target might not exist *)
  ignore (run ~proc_mgr ~cwd ["rm"; "-rf"; target])

let rm_cached_rf ~proc_mgr ~cwd =
  Log.debug (fun m -> m "Removing all files from index");
  (* Ignore errors - index might be empty *)
  ignore (run ~proc_mgr ~cwd ["rm"; "-rf"; "--cached"; "."])

let add_all ~proc_mgr ~cwd =
  Log.debug (fun m -> m "Staging all changes");
  run_exn ~proc_mgr ~cwd ["add"; "-A"] |> ignore

let commit ~proc_mgr ~cwd ~message =
  Log.debug (fun m -> m "Committing: %s" (String.sub message 0 (min 50 (String.length message))));
  run_exn ~proc_mgr ~cwd ["commit"; "-m"; message] |> ignore

let commit_allow_empty ~proc_mgr ~cwd ~message =
  Log.debug (fun m -> m "Committing (allow empty): %s" (String.sub message 0 (min 50 (String.length message))));
  run_exn ~proc_mgr ~cwd ["commit"; "--allow-empty"; "-m"; message] |> ignore

let branch_create ~proc_mgr ~cwd ~name ~start_point =
  Log.info (fun m -> m "Creating branch %s at %s" name start_point);
  run_exn ~proc_mgr ~cwd ["branch"; name; start_point] |> ignore

let branch_force ~proc_mgr ~cwd ~name ~point =
  Log.info (fun m -> m "Force-moving branch %s to %s" name point);
  run_exn ~proc_mgr ~cwd ["branch"; "-f"; name; point] |> ignore

let remote_add ~proc_mgr ~cwd ~name ~url =
  Log.info (fun m -> m "Adding remote %s -> %s" name url);
  run_exn ~proc_mgr ~cwd ["remote"; "add"; name; url] |> ignore

let remote_set_url ~proc_mgr ~cwd ~name ~url =
  Log.info (fun m -> m "Setting remote %s URL to %s" name url);
  run_exn ~proc_mgr ~cwd ["remote"; "set-url"; name; url] |> ignore

let merge_allow_unrelated ~proc_mgr ~cwd ~branch ~message =
  Log.info (fun m -> m "Merging %s (allow unrelated histories)..." branch);
  match run ~proc_mgr ~cwd ["merge"; "--allow-unrelated-histories"; "-m"; message; branch] with
  | Ok _ -> Ok ()
  | Error (Command_failed { exit_code = 1; _ }) ->
      (* Merge conflict - get list of conflicting files *)
      let output = run_exn ~proc_mgr ~cwd ["diff"; "--name-only"; "--diff-filter=U"] in
      let files = lines output in
      Log.warn (fun m -> m "Merge conflict: %a" Fmt.(list ~sep:comma string) files);
      Error (`Conflict files)
  | Error e ->
      raise (err e)

let rebase ~proc_mgr ~cwd ~onto =
  Log.info (fun m -> m "Rebasing onto %s..." onto);
  match run ~proc_mgr ~cwd ["rebase"; onto] with
  | Ok _ -> Ok ()
  | Error (Command_failed { stderr; _ }) ->
      let hint =
        if String.length stderr > 200 then
          String.sub stderr 0 200 ^ "..."
        else
          stderr
      in
      Log.warn (fun m -> m "Rebase conflict onto %s" onto);
      Error (`Conflict hint)
  | Error e ->
      raise (err e)

let rebase_abort ~proc_mgr ~cwd =
  Log.info (fun m -> m "Aborting rebase...");
  ignore (run ~proc_mgr ~cwd ["rebase"; "--abort"])

let merge_abort ~proc_mgr ~cwd =
  Log.info (fun m -> m "Aborting merge...");
  ignore (run ~proc_mgr ~cwd ["merge"; "--abort"])

let reset_hard ~proc_mgr ~cwd ref_ =
  Log.info (fun m -> m "Hard reset to %s" ref_);
  run_exn ~proc_mgr ~cwd ["reset"; "--hard"; ref_] |> ignore

let clean_fd ~proc_mgr ~cwd =
  Log.debug (fun m -> m "Cleaning untracked files");
  run_exn ~proc_mgr ~cwd ["clean"; "-fd"] |> ignore

let filter_repo_to_subdirectory ~proc_mgr ~cwd ~branch ~subdirectory =
  Log.info (fun m -> m "Rewriting history of %s into subdirectory %s..." branch subdirectory);
  (* Use git-filter-repo with --to-subdirectory-filter to rewrite all paths into subdirectory.
     This preserves full history with paths prefixed. Much faster than filter-branch.

     For bare repositories, we need to create a temporary worktree, run filter-repo
     there, and then update the branch in the bare repo. *)

  (* Create a unique temporary worktree name using the branch name *)
  let safe_branch = String.map (fun c -> if c = '/' then '-' else c) branch in
  let temp_wt_name = ".filter-tmp-" ^ safe_branch in
  let temp_wt_relpath = "../" ^ temp_wt_name in

  (* Construct the worktree path - cwd is (fs, path_string), so we go up one level *)
  let fs = fst cwd in
  let git_path = snd cwd in
  let parent_path = Filename.dirname git_path in
  let temp_wt_path = Filename.concat parent_path temp_wt_name in
  let temp_wt : path = (fs, temp_wt_path) in

  (* Remove any existing temp worktree *)
  ignore (run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);

  (* Create worktree for the branch *)
  run_exn ~proc_mgr ~cwd ["worktree"; "add"; temp_wt_relpath; branch] |> ignore;

  (* Run git-filter-repo in the worktree *)
  let result = run ~proc_mgr ~cwd:temp_wt [
    "filter-repo";
    "--to-subdirectory-filter"; subdirectory;
    "--force";
    "--refs"; "HEAD"
  ] in

  (* Handle result: get the new SHA, cleanup worktree, then update branch *)
  (match result with
   | Ok _ ->
       (* Get the new HEAD SHA from the worktree BEFORE removing it *)
       let new_sha = run_exn ~proc_mgr ~cwd:temp_wt ["rev-parse"; "HEAD"] |> string_trim in
       (* Cleanup temporary worktree first (must do this before updating branch) *)
       ignore (run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);
       (* Now update the branch in the bare repo *)
       run_exn ~proc_mgr ~cwd ["branch"; "-f"; branch; new_sha] |> ignore
   | Error e ->
       (* Cleanup and re-raise *)
       ignore (run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);
       raise (err e))

let filter_repo_from_subdirectory ~proc_mgr ~cwd ~branch ~subdirectory =
  Log.info (fun m -> m "Extracting %s from subdirectory %s to root..." branch subdirectory);
  (* Use git-filter-repo with --subdirectory-filter to extract files from subdirectory
     to root. This is the inverse of --to-subdirectory-filter.
     Preserves history for files that were in the subdirectory.

     For bare repositories, we need to create a temporary worktree, run filter-repo
     there, and then update the branch in the bare repo. *)

  (* Create a unique temporary worktree name using the branch name *)
  let safe_branch = String.map (fun c -> if c = '/' then '-' else c) branch in
  let temp_wt_name = ".filter-tmp-" ^ safe_branch in
  let temp_wt_relpath = "../" ^ temp_wt_name in

  (* Construct the worktree path - cwd is (fs, path_string), so we go up one level *)
  let fs = fst cwd in
  let git_path = snd cwd in
  let parent_path = Filename.dirname git_path in
  let temp_wt_path = Filename.concat parent_path temp_wt_name in
  let temp_wt : path = (fs, temp_wt_path) in

  (* Remove any existing temp worktree *)
  ignore (run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);

  (* Create worktree for the branch *)
  run_exn ~proc_mgr ~cwd ["worktree"; "add"; temp_wt_relpath; branch] |> ignore;

  (* Run git-filter-repo in the worktree with --subdirectory-filter *)
  let result = run ~proc_mgr ~cwd:temp_wt [
    "filter-repo";
    "--subdirectory-filter"; subdirectory;
    "--force";
    "--refs"; "HEAD"
  ] in

  (* Handle result: get the new SHA, cleanup worktree, then update branch *)
  (match result with
   | Ok _ ->
       (* Get the new HEAD SHA from the worktree BEFORE removing it *)
       let new_sha = run_exn ~proc_mgr ~cwd:temp_wt ["rev-parse"; "HEAD"] |> string_trim in
       (* Cleanup temporary worktree first (must do this before updating branch) *)
       ignore (run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);
       (* Now update the branch in the bare repo *)
       run_exn ~proc_mgr ~cwd ["branch"; "-f"; branch; new_sha] |> ignore
   | Error e ->
       (* Cleanup and re-raise *)
       ignore (run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);
       raise (err e))
