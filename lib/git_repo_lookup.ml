(** Git repository URL lookup and rewriting.

    This module handles URL rewriting for git repositories, mapping known
    slow upstream URLs to faster mirrors, and branch/tag overrides for
    specific packages. *)

(** Rewrite a git URL to use a faster mirror if available.

    Currently handles:
    - erratique.ch repos are mirrored on GitHub under dbuenzli
    - git.robur.coop repos are mirrored on GitHub under robur-coop
      (strips the org prefix: git.robur.coop/robur/X -> github.com/robur-coop/X) *)
let rewrite_url url =
  (* Helper to check and rewrite prefix *)
  let try_rewrite ~prefix ~replacement url =
    if String.length url > String.length prefix
       && String.sub url 0 (String.length prefix) = prefix
    then
      let rest = String.sub url (String.length prefix)
          (String.length url - String.length prefix) in
      Some (replacement ^ rest)
    else None
  in
  (* Helper to rewrite robur.coop URLs, stripping the org path component *)
  let try_rewrite_robur ~prefix url =
    if String.length url > String.length prefix
       && String.sub url 0 (String.length prefix) = prefix
    then
      (* rest is e.g. "robur/ohex.git" - strip org prefix *)
      let rest = String.sub url (String.length prefix)
          (String.length url - String.length prefix) in
      (* Find the first slash to strip the org *)
      match String.index_opt rest '/' with
      | Some idx ->
          let repo = String.sub rest (idx + 1) (String.length rest - idx - 1) in
          Some ("https://github.com/robur-coop/" ^ repo)
      | None -> Some ("https://github.com/robur-coop/" ^ rest)
    else None
  in
  (* Try each rewrite rule in order *)
  match try_rewrite ~prefix:"https://erratique.ch/repos/"
          ~replacement:"https://github.com/dbuenzli/" url with
  | Some u -> u
  | None ->
  match try_rewrite ~prefix:"http://erratique.ch/repos/"
          ~replacement:"https://github.com/dbuenzli/" url with
  | Some u -> u
  | None ->
  match try_rewrite_robur ~prefix:"https://git.robur.coop/" url with
  | Some u -> u
  | None ->
  match try_rewrite_robur ~prefix:"git://git.robur.coop/" url with
  | Some u -> u
  | None -> url

(** Override branch/tag for specific packages.

    Some packages have unstable main branches or we want to pin to specific
    versions. This returns Some ref if an override exists, None otherwise.

    Currently handles:
    - dune: use tag 3.20.2 instead of main branch *)
let branch_override ~name ~url =
  (* Dune's main branch can be unstable; pin to release tag *)
  let is_dune_url =
    String.equal url "https://github.com/ocaml/dune.git" ||
    String.equal url "https://github.com/ocaml/dune" ||
    String.equal url "git://github.com/ocaml/dune.git"
  in
  if name = "dune" || is_dune_url then
    Some "3.20.2"
  else
    None
