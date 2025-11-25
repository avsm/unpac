let src = Logs.Src.create "claude.options" ~doc:"Claude configuration options"

module Log = (val Logs.src_log src : Logs.LOG)

type setting_source = User | Project | Local

type t = {
  allowed_tools : string list;
  disallowed_tools : string list;
  max_thinking_tokens : int;
  system_prompt : string option;
  append_system_prompt : string option;
  permission_mode : Permissions.Mode.t option;
  permission_callback : Permissions.callback option;
  model : Model.t option;
  cwd : Eio.Fs.dir_ty Eio.Path.t option;
  env : (string * string) list;
  continue_conversation : bool;
  resume : string option;
  max_turns : int option;
  permission_prompt_tool_name : string option;
  settings : string option;
  add_dirs : string list;
  extra_args : (string * string option) list;
  debug_stderr : Eio.Flow.sink_ty Eio.Flow.sink option;
  hooks : Hooks.config option;
  max_budget_usd : float option;
  fallback_model : Model.t option;
  setting_sources : setting_source list option;
  max_buffer_size : int option;
  user : string option;
  output_format : Structured_output.t option;
  unknown : Unknown.t;
}

let default =
  {
    allowed_tools = [];
    disallowed_tools = [];
    max_thinking_tokens = 8000;
    system_prompt = None;
    append_system_prompt = None;
    permission_mode = None;
    permission_callback = Some Permissions.default_allow_callback;
    model = None;
    cwd = None;
    env = [];
    continue_conversation = false;
    resume = None;
    max_turns = None;
    permission_prompt_tool_name = None;
    settings = None;
    add_dirs = [];
    extra_args = [];
    debug_stderr = None;
    hooks = None;
    max_budget_usd = None;
    fallback_model = None;
    setting_sources = None;
    max_buffer_size = None;
    user = None;
    output_format = None;
    unknown = Unknown.empty;
  }

let create ?(allowed_tools = []) ?(disallowed_tools = [])
    ?(max_thinking_tokens = 8000) ?system_prompt ?append_system_prompt
    ?permission_mode ?permission_callback ?model ?cwd ?(env = [])
    ?(continue_conversation = false) ?resume ?max_turns
    ?permission_prompt_tool_name ?settings ?(add_dirs = []) ?(extra_args = [])
    ?debug_stderr ?hooks ?max_budget_usd ?fallback_model ?setting_sources
    ?max_buffer_size ?user ?output_format ?(unknown = Unknown.empty) () =
  {
    allowed_tools;
    disallowed_tools;
    max_thinking_tokens;
    system_prompt;
    append_system_prompt;
    permission_mode;
    permission_callback;
    model;
    cwd;
    env;
    continue_conversation;
    resume;
    max_turns;
    permission_prompt_tool_name;
    settings;
    add_dirs;
    extra_args;
    debug_stderr;
    hooks;
    max_budget_usd;
    fallback_model;
    setting_sources;
    max_buffer_size;
    user;
    output_format;
    unknown;
  }

let allowed_tools t = t.allowed_tools
let disallowed_tools t = t.disallowed_tools
let max_thinking_tokens t = t.max_thinking_tokens
let system_prompt t = t.system_prompt
let append_system_prompt t = t.append_system_prompt
let permission_mode t = t.permission_mode
let permission_callback t = t.permission_callback
let model t = t.model
let cwd t = t.cwd
let env t = t.env
let continue_conversation t = t.continue_conversation
let resume t = t.resume
let max_turns t = t.max_turns
let permission_prompt_tool_name t = t.permission_prompt_tool_name
let settings t = t.settings
let add_dirs t = t.add_dirs
let extra_args t = t.extra_args
let debug_stderr t = t.debug_stderr
let hooks t = t.hooks
let max_budget_usd t = t.max_budget_usd
let fallback_model t = t.fallback_model
let setting_sources t = t.setting_sources
let max_buffer_size t = t.max_buffer_size
let user t = t.user
let output_format t = t.output_format
let unknown t = t.unknown
let with_allowed_tools tools t = { t with allowed_tools = tools }
let with_disallowed_tools tools t = { t with disallowed_tools = tools }
let with_max_thinking_tokens tokens t = { t with max_thinking_tokens = tokens }
let with_system_prompt prompt t = { t with system_prompt = Some prompt }

let with_append_system_prompt prompt t =
  { t with append_system_prompt = Some prompt }

let with_permission_mode mode t = { t with permission_mode = Some mode }

let with_permission_callback callback t =
  { t with permission_callback = Some callback }

let with_model model t = { t with model = Some model }
let with_model_string model t = { t with model = Some (Model.of_string model) }
let with_cwd cwd t = { t with cwd = Some cwd }
let with_env env t = { t with env }

let with_continue_conversation continue t =
  { t with continue_conversation = continue }

let with_resume session_id t = { t with resume = Some session_id }
let with_max_turns turns t = { t with max_turns = Some turns }

let with_permission_prompt_tool_name tool t =
  { t with permission_prompt_tool_name = Some tool }

let with_settings path t = { t with settings = Some path }
let with_add_dirs dirs t = { t with add_dirs = dirs }
let with_extra_args args t = { t with extra_args = args }
let with_debug_stderr sink t = { t with debug_stderr = Some sink }
let with_hooks hooks t = { t with hooks = Some hooks }
let with_max_budget_usd budget t = { t with max_budget_usd = Some budget }
let with_fallback_model model t = { t with fallback_model = Some model }

let with_fallback_model_string model t =
  { t with fallback_model = Some (Model.of_string model) }

let with_setting_sources sources t = { t with setting_sources = Some sources }
let with_no_settings t = { t with setting_sources = Some [] }
let with_max_buffer_size size t = { t with max_buffer_size = Some size }
let with_user user t = { t with user = Some user }
let with_output_format format t = { t with output_format = Some format }

(* Helper codec for Model.t *)
let model_jsont : Model.t Jsont.t =
  Jsont.map ~kind:"Model" ~dec:Model.of_string ~enc:Model.to_string Jsont.string

(* Helper codec for env - list of string pairs encoded as object.
   Env is a dynamic object where all values should be strings.
   Uses pattern matching to extract object members, then jsont for string decoding. *)
let env_jsont : (string * string) list Jsont.t =
  Jsont.map ~kind:"Env"
    ~dec:(fun json ->
      match json with
      | Jsont.Object (members, _) ->
          List.filter_map
            (fun ((name, _), value) ->
              match Jsont.Json.decode Jsont.string value with
              | Ok s -> Some (name, s)
              | Error _ -> None)
            members
      | _ -> [])
    ~enc:(fun pairs ->
      Jsont.Json.object'
        (List.map
           (fun (k, v) ->
             Jsont.Json.mem (Jsont.Json.name k) (Jsont.Json.string v))
           pairs))
    Jsont.json

let jsont : t Jsont.t =
  let make allowed_tools disallowed_tools max_thinking_tokens system_prompt
      append_system_prompt permission_mode model env unknown =
    {
      allowed_tools;
      disallowed_tools;
      max_thinking_tokens;
      system_prompt;
      append_system_prompt;
      permission_mode;
      permission_callback = Some Permissions.default_allow_callback;
      model;
      cwd = None;
      env;
      continue_conversation = false;
      resume = None;
      max_turns = None;
      permission_prompt_tool_name = None;
      settings = None;
      add_dirs = [];
      extra_args = [];
      debug_stderr = None;
      hooks = None;
      max_budget_usd = None;
      fallback_model = None;
      setting_sources = None;
      max_buffer_size = None;
      user = None;
      output_format = None;
      unknown;
    }
  in
  Jsont.Object.(
    map ~kind:"Options" make
    |> mem "allowed_tools" (Jsont.list Jsont.string) ~enc:allowed_tools
         ~dec_absent:[]
    |> mem "disallowed_tools" (Jsont.list Jsont.string) ~enc:disallowed_tools
         ~dec_absent:[]
    |> mem "max_thinking_tokens" Jsont.int ~enc:max_thinking_tokens
         ~dec_absent:8000
    |> opt_mem "system_prompt" Jsont.string ~enc:system_prompt
    |> opt_mem "append_system_prompt" Jsont.string ~enc:append_system_prompt
    |> opt_mem "permission_mode" Permissions.Mode.jsont ~enc:permission_mode
    |> opt_mem "model" model_jsont ~enc:model
    |> mem "env" env_jsont ~enc:env ~dec_absent:[]
    |> keep_unknown Jsont.json_mems ~enc:unknown
    |> finish)

let log_options t =
  Log.debug (fun m -> m "Claude options: %a" (Jsont.pp_value jsont ()) t)
