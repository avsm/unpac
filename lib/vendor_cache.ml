(** Vendor cache - a persistent bare git repository for caching upstream fetches.

    The cache stores fetched repositories as remotes/branches, allowing multiple
    unpac projects to share fetched content without re-downloading. *)

(** {1 Types} *)

type t = Eio.Fs.dir_ty Eio.Path.t
(** Path to the cache bare repository *)

(** {1 Cache Location} *)

let default_path () =
  let cache_home =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some dir -> dir
    | None ->
        match Sys.getenv_opt "HOME" with
        | Some home -> Filename.concat home ".cache"
        | None -> "/tmp"
  in
  Filename.concat cache_home "unpac/vendor-cache"

(** {1 Initialization} *)

let init ~proc_mgr ~fs ?path () =
  let cache_path = match path with
    | Some p -> p
    | None -> default_path ()
  in
  let cache = Eio.Path.(fs / cache_path) in

  (* Check if already initialized *)
  if Eio.Path.is_directory cache then
    cache
  else begin
    (* Create parent directories *)
    let parent = Filename.dirname cache_path in
    let parent_path = Eio.Path.(fs / parent) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent_path;

    (* Initialize bare repository *)
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o755 cache;
    Git.run_exn ~proc_mgr ~cwd:cache ["init"; "--bare"] |> ignore;
    cache
  end

(** {1 Remote Naming}

    We use URL-based remote names to avoid conflicts.
    e.g., "github.com/dbuenzli/astring" for https://github.com/dbuenzli/astring.git *)

let url_to_remote_name url =
  (* Strip protocol and .git suffix *)
  let url =
    let prefixes = ["https://"; "http://"; "git://"; "ssh://"; "git@"] in
    List.fold_left (fun u prefix ->
      if String.starts_with ~prefix u then
        String.sub u (String.length prefix) (String.length u - String.length prefix)
      else u
    ) url prefixes
  in
  let url =
    if String.ends_with ~suffix:".git" url then
      String.sub url 0 (String.length url - 4)
    else url
  in
  (* Replace : with / for git@ style URLs *)
  String.map (fun c -> if c = ':' then '/' else c) url

let branch_name ~remote ~branch =
  remote ^ "/" ^ branch

(** {1 Cache Operations} *)

let has_remote ~proc_mgr cache remote_name =
  match Git.remote_url ~proc_mgr ~cwd:cache remote_name with
  | Some _ -> true
  | None -> false

let ensure_remote ~proc_mgr cache ~url =
  let remote_name = url_to_remote_name url in
  if has_remote ~proc_mgr cache remote_name then
    remote_name
  else begin
    Git.run_exn ~proc_mgr ~cwd:cache
      ["remote"; "add"; remote_name; url] |> ignore;
    remote_name
  end

let fetch ~proc_mgr cache ~url =
  let remote_name = ensure_remote ~proc_mgr cache ~url in
  Git.fetch ~proc_mgr ~cwd:cache ~remote:remote_name;
  remote_name

let get_ref ~proc_mgr cache ~url ~branch =
  let remote_name = url_to_remote_name url in
  let ref_name = branch_name ~remote:remote_name ~branch in
  match Git.rev_parse ~proc_mgr ~cwd:cache ref_name with
  | Some sha -> Some sha
  | None -> None

(** Fetch to cache, then clone ref into project's bare repo *)
let fetch_to_project ~proc_mgr ~cache ~project_git ~url ~branch =
  (* First, fetch to cache (include tags, force update to avoid conflicts) *)
  let remote_name = ensure_remote ~proc_mgr cache ~url in
  Git.run_exn ~proc_mgr ~cwd:cache
    ["fetch"; "--tags"; "--force"; remote_name] |> ignore;

  (* Determine if this is a branch or tag *)
  let branch_ref = branch_name ~remote:remote_name ~branch in
  let tag_ref = "refs/tags/" ^ branch in

  (* Check which ref exists in cache *)
  let cache_ref =
    match Git.rev_parse ~proc_mgr ~cwd:cache branch_ref with
    | Some _ -> branch_ref
    | None ->
        (* Try as a tag *)
        match Git.rev_parse ~proc_mgr ~cwd:cache tag_ref with
        | Some _ -> tag_ref
        | None -> failwith (Printf.sprintf "Ref not found: %s (tried branch %s and tag %s)"
                              branch branch_ref tag_ref)
  in

  (* Now fetch from cache into project *)
  let cache_path = snd cache in

  (* Add cache as a remote in project if not exists *)
  let cache_remote = "vendor-cache" in
  (match Git.remote_url ~proc_mgr ~cwd:project_git cache_remote with
   | None ->
       Git.run_exn ~proc_mgr ~cwd:project_git
         ["remote"; "add"; cache_remote; cache_path] |> ignore
   | Some _ -> ());

  (* Fetch the specific ref from cache *)
  Git.run_exn ~proc_mgr ~cwd:project_git
    ["fetch"; cache_remote; cache_ref ^ ":" ^ cache_ref] |> ignore;

  cache_ref

(** {1 Listing} *)

let list_remotes ~proc_mgr cache =
  Git.run_lines ~proc_mgr ~cwd:cache ["remote"]

let list_branches ~proc_mgr cache =
  Git.run_lines ~proc_mgr ~cwd:cache ["branch"; "-a"]
  |> List.filter_map (fun line ->
      let line = String.trim line in
      if String.starts_with ~prefix:"* " line then
        Some (String.sub line 2 (String.length line - 2))
      else if line <> "" then
        Some line
      else
        None)
