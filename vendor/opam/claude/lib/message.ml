let src = Logs.Src.create "claude.message" ~doc:"Claude messages"
module Log = (val Logs.src_log src : Logs.LOG)


module User = struct
  type content =
    | String of string
    | Blocks of Content_block.t list

  type t = {
    content : content;
    unknown : Unknown.t;
  }

  let create_string s = { content = String s; unknown = Unknown.empty }
  let create_blocks blocks = { content = Blocks blocks; unknown = Unknown.empty }

  let create_with_tool_result ~tool_use_id ~content ?is_error () =
    let tool_result = Content_block.tool_result ~tool_use_id ~content ?is_error () in
    { content = Blocks [tool_result]; unknown = Unknown.empty }

  let create_mixed ~text ~tool_results =
    let blocks =
      let text_blocks = match text with
        | Some t -> [Content_block.text t]
        | None -> []
      in
      let tool_blocks = List.map (fun (tool_use_id, content, is_error) ->
        Content_block.tool_result ~tool_use_id ~content ?is_error ()
      ) tool_results in
      text_blocks @ tool_blocks
    in
    { content = Blocks blocks; unknown = Unknown.empty }

  let make content unknown = { content; unknown }
  let content t = t.content
  let unknown t = t.unknown

  let as_text t = match t.content with
    | String s -> Some s
    | Blocks _ -> None

  let get_blocks t = match t.content with
    | String s -> [Content_block.text s]
    | Blocks blocks -> blocks

  (* Decode content from json value *)
  let decode_content json = match json with
    | Jsont.String (s, _) -> String s
    | Jsont.Array (items, _) ->
        let blocks = List.map (fun j ->
          match Jsont.Json.decode Content_block.jsont j with
          | Ok b -> b
          | Error msg -> failwith ("Invalid content block: " ^ msg)
        ) items in
        Blocks blocks
    | _ -> failwith "Content must be string or array"

  (* Encode content to json value *)
  let encode_content = function
    | String s -> Jsont.String (s, Jsont.Meta.none)
    | Blocks blocks -> Jsont.Array (List.map Content_block.to_json blocks, Jsont.Meta.none)

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"User" (fun json_content unknown ->
      let content = decode_content json_content in
      make content unknown
    )
    |> Jsont.Object.mem "content" Jsont.json ~enc:(fun t -> encode_content (content t))
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let to_json t =
    let content_json = match t.content with
      | String s -> Jsont.String (s, Jsont.Meta.none)
      | Blocks blocks ->
          Jsont.Array (List.map Content_block.to_json blocks, Jsont.Meta.none)
    in
    Jsont.Object ([
      (Jsont.Json.name "type", Jsont.String ("user", Jsont.Meta.none));
      (Jsont.Json.name "message", Jsont.Object ([
        (Jsont.Json.name "role", Jsont.String ("user", Jsont.Meta.none));
        (Jsont.Json.name "content", content_json);
      ], Jsont.Meta.none));
    ], Jsont.Meta.none)

  (* Jsont codec for parsing incoming user messages from CLI *)
  let incoming_jsont : t Jsont.t =
    let message_jsont =
      Jsont.Object.map ~kind:"UserMessage" (fun json_content ->
        let content = decode_content json_content in
        { content; unknown = Unknown.empty }
      )
      |> Jsont.Object.mem "content" Jsont.json ~enc:(fun t -> encode_content (content t))
      |> Jsont.Object.finish
    in
    Jsont.Object.map ~kind:"UserEnvelope" Fun.id
    |> Jsont.Object.mem "message" message_jsont ~enc:Fun.id
    |> Jsont.Object.finish

  let of_json json =
    match Jsont.Json.decode incoming_jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("User.of_json: " ^ msg))

  let pp fmt t =
    match t.content with
    | String s ->
        if String.length s > 60 then
          let truncated = String.sub s 0 57 in
          Fmt.pf fmt "@[<2>User:@ %s...@]" truncated
        else
          Fmt.pf fmt "@[<2>User:@ %S@]" s
    | Blocks blocks ->
        let text_count = List.length (List.filter (function
          | Content_block.Text _ -> true | _ -> false) blocks) in
        let tool_result_count = List.length (List.filter (function
          | Content_block.Tool_result _ -> true | _ -> false) blocks) in
        match text_count, tool_result_count with
        | 1, 0 ->
            let text = List.find_map (function
              | Content_block.Text t -> Some (Content_block.Text.text t)
              | _ -> None) blocks in
            Fmt.pf fmt "@[<2>User:@ %a@]" Fmt.(option string) text
        | 0, 1 ->
            Fmt.pf fmt "@[<2>User:@ [tool result]@]"
        | 0, n when n > 1 ->
            Fmt.pf fmt "@[<2>User:@ [%d tool results]@]" n
        | _ ->
            Fmt.pf fmt "@[<2>User:@ [%d blocks]@]" (List.length blocks)
end

module Assistant = struct
  type error = [
    | `Authentication_failed
    | `Billing_error
    | `Rate_limit
    | `Invalid_request
    | `Server_error
    | `Unknown
  ]

  let error_to_string = function
    | `Authentication_failed -> "authentication_failed"
    | `Billing_error -> "billing_error"
    | `Rate_limit -> "rate_limit"
    | `Invalid_request -> "invalid_request"
    | `Server_error -> "server_error"
    | `Unknown -> "unknown"

  let error_of_string = function
    | "authentication_failed" -> `Authentication_failed
    | "billing_error" -> `Billing_error
    | "rate_limit" -> `Rate_limit
    | "invalid_request" -> `Invalid_request
    | "server_error" -> `Server_error
    | "unknown" | _ -> `Unknown

  let error_jsont : error Jsont.t =
    Jsont.enum [
      ("authentication_failed", `Authentication_failed);
      ("billing_error", `Billing_error);
      ("rate_limit", `Rate_limit);
      ("invalid_request", `Invalid_request);
      ("server_error", `Server_error);
      ("unknown", `Unknown);
    ]

  type t = {
    content : Content_block.t list;
    model : string;
    error : error option;
    unknown : Unknown.t;
  }

  let create ~content ~model ?error () = { content; model; error; unknown = Unknown.empty }
  let make content model error unknown = { content; model; error; unknown }
  let content t = t.content
  let model t = t.model
  let error t = t.error
  let unknown t = t.unknown

  let get_text_blocks t =
    List.filter_map (function
      | Content_block.Text text -> Some (Content_block.Text.text text)
      | _ -> None
    ) t.content

  let get_tool_uses t =
    List.filter_map (function
      | Content_block.Tool_use tool -> Some tool
      | _ -> None
    ) t.content

  let get_thinking t =
    List.filter_map (function
      | Content_block.Thinking thinking -> Some thinking
      | _ -> None
    ) t.content

  let has_tool_use t =
    List.exists (function
      | Content_block.Tool_use _ -> true
      | _ -> false
    ) t.content

  let combined_text t =
    String.concat "\n" (get_text_blocks t)

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"Assistant" make
    |> Jsont.Object.mem "content" (Jsont.list Content_block.jsont) ~enc:content
    |> Jsont.Object.mem "model" Jsont.string ~enc:model
    |> Jsont.Object.opt_mem "error" error_jsont ~enc:error
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let to_json t =
    let msg_fields = [
      (Jsont.Json.name "content", Jsont.Array (List.map Content_block.to_json t.content, Jsont.Meta.none));
      (Jsont.Json.name "model", Jsont.String (t.model, Jsont.Meta.none));
    ] in
    let msg_fields = match t.error with
      | Some err -> (Jsont.Json.name "error", Jsont.String (error_to_string err, Jsont.Meta.none)) :: msg_fields
      | None -> msg_fields
    in
    Jsont.Object ([
      (Jsont.Json.name "type", Jsont.String ("assistant", Jsont.Meta.none));
      (Jsont.Json.name "message", Jsont.Object (msg_fields, Jsont.Meta.none));
    ], Jsont.Meta.none)

  (* Jsont codec for parsing incoming assistant messages from CLI *)
  let incoming_jsont : t Jsont.t =
    Jsont.Object.map ~kind:"AssistantEnvelope" Fun.id
    |> Jsont.Object.mem "message" jsont ~enc:Fun.id
    |> Jsont.Object.finish

  let of_json json =
    match Jsont.Json.decode incoming_jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("Assistant.of_json: " ^ msg))

  let pp fmt t =
    let text_count = List.length (get_text_blocks t) in
    let tool_count = List.length (get_tool_uses t) in
    let thinking_count = List.length (get_thinking t) in
    match text_count, tool_count, thinking_count with
    | 1, 0, 0 ->
        (* Simple text response *)
        Fmt.pf fmt "@[<2>Assistant@ [%s]:@ %S@]"
          t.model (combined_text t)
    | _, 0, 0 when text_count > 0 ->
        (* Multiple text blocks *)
        Fmt.pf fmt "@[<2>Assistant@ [%s]:@ %d text blocks@]"
          t.model text_count
    | 0, _, 0 when tool_count > 0 ->
        (* Only tool uses *)
        let tools = get_tool_uses t in
        let tool_names = List.map Content_block.Tool_use.name tools in
        Fmt.pf fmt "@[<2>Assistant@ [%s]:@ tools(%a)@]"
          t.model Fmt.(list ~sep:comma string) tool_names
    | _ ->
        (* Mixed content *)
        let parts = [] in
        let parts = if text_count > 0 then
          Printf.sprintf "%d text" text_count :: parts else parts in
        let parts = if tool_count > 0 then
          Printf.sprintf "%d tools" tool_count :: parts else parts in
        let parts = if thinking_count > 0 then
          Printf.sprintf "%d thinking" thinking_count :: parts else parts in
        Fmt.pf fmt "@[<2>Assistant@ [%s]:@ %s@]"
          t.model (String.concat ", " (List.rev parts))
end

module System = struct
  (** System messages as a discriminated union on "subtype" field *)

  type init = {
    session_id : string option;
    model : string option;
    cwd : string option;
    unknown : Unknown.t;
  }

  type error = {
    error : string;
    unknown : Unknown.t;
  }

  type other = {
    subtype : string;
    unknown : Unknown.t;
  }

  type t =
    | Init of init
    | Error of error
    | Other of other

  (* Accessors *)
  let session_id = function Init i -> i.session_id | _ -> None
  let model = function Init i -> i.model | _ -> None
  let cwd = function Init i -> i.cwd | _ -> None
  let error_msg = function Error e -> Some e.error | _ -> None
  let subtype = function Init _ -> "init" | Error _ -> "error" | Other o -> o.subtype
  let unknown = function
    | Init i -> i.unknown
    | Error e -> e.unknown
    | Other o -> o.unknown

  (* Constructors *)
  let init ?session_id ?model ?cwd () =
    Init { session_id; model; cwd; unknown = Unknown.empty }

  let error ~error =
    Error { error; unknown = Unknown.empty }

  let other ~subtype =
    Other { subtype; unknown = Unknown.empty }

  (* Individual record codecs *)
  let init_jsont : init Jsont.t =
    let make session_id model cwd unknown : init = { session_id; model; cwd; unknown } in
    Jsont.Object.map ~kind:"SystemInit" make
    |> Jsont.Object.opt_mem "session_id" Jsont.string ~enc:(fun (r : init) -> r.session_id)
    |> Jsont.Object.opt_mem "model" Jsont.string ~enc:(fun (r : init) -> r.model)
    |> Jsont.Object.opt_mem "cwd" Jsont.string ~enc:(fun (r : init) -> r.cwd)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : init) -> r.unknown)
    |> Jsont.Object.finish

  let error_jsont : error Jsont.t =
    let make err unknown : error = { error = err; unknown } in
    Jsont.Object.map ~kind:"SystemError" make
    |> Jsont.Object.mem "error" Jsont.string ~enc:(fun (r : error) -> r.error)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : error) -> r.unknown)
    |> Jsont.Object.finish

  (* Main codec using case_mem for "subtype" discriminator *)
  let jsont : t Jsont.t =
    let case_init = Jsont.Object.Case.map "init" init_jsont ~dec:(fun v -> Init v) in
    let case_error = Jsont.Object.Case.map "error" error_jsont ~dec:(fun v -> Error v) in
    let case_other tag =
      (* For unknown subtypes, create Other with the tag as subtype *)
      let other_codec : other Jsont.t =
        let make unknown : other = { subtype = tag; unknown } in
        Jsont.Object.map ~kind:"SystemOther" make
        |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : other) -> r.unknown)
        |> Jsont.Object.finish
      in
      Jsont.Object.Case.map tag other_codec ~dec:(fun v -> Other v)
    in
    let enc_case = function
      | Init v -> Jsont.Object.Case.value case_init v
      | Error v -> Jsont.Object.Case.value case_error v
      | Other v -> Jsont.Object.Case.value (case_other v.subtype) v
    in
    let cases = Jsont.Object.Case.[
      make case_init;
      make case_error;
    ] in
    Jsont.Object.map ~kind:"System" Fun.id
    |> Jsont.Object.case_mem "subtype" Jsont.string ~enc:Fun.id ~enc_case cases
        ~tag_to_string:Fun.id ~tag_compare:String.compare
    |> Jsont.Object.finish

  let to_json t =
    match Jsont.Json.encode jsont t with
    | Ok json -> json
    | Error msg -> failwith ("System.to_json: " ^ msg)

  let of_json json =
    match Jsont.Json.decode jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("System.of_json: " ^ msg))

  let pp fmt = function
    | Init i ->
        Fmt.pf fmt "@[<2>System.init@ { session_id = %a;@ model = %a;@ cwd = %a }@]"
          Fmt.(option string) i.session_id
          Fmt.(option string) i.model
          Fmt.(option string) i.cwd
    | Error e ->
        Fmt.pf fmt "@[<2>System.error@ { error = %s }@]" e.error
    | Other o ->
        Fmt.pf fmt "@[<2>System.%s@ { ... }@]" o.subtype
end

module Result = struct
  module Usage = struct
    type t = {
      input_tokens : int option;
      output_tokens : int option;
      total_tokens : int option;
      cache_creation_input_tokens : int option;
      cache_read_input_tokens : int option;
      unknown : Unknown.t;
    }

    let make input_tokens output_tokens total_tokens
        cache_creation_input_tokens cache_read_input_tokens unknown =
      { input_tokens; output_tokens; total_tokens;
        cache_creation_input_tokens; cache_read_input_tokens; unknown }

    let create ?input_tokens ?output_tokens ?total_tokens
               ?cache_creation_input_tokens ?cache_read_input_tokens () =
      { input_tokens; output_tokens; total_tokens;
        cache_creation_input_tokens; cache_read_input_tokens;
        unknown = Unknown.empty }

    let input_tokens t = t.input_tokens
    let output_tokens t = t.output_tokens
    let total_tokens t = t.total_tokens
    let cache_creation_input_tokens t = t.cache_creation_input_tokens
    let cache_read_input_tokens t = t.cache_read_input_tokens
    let unknown t = t.unknown

    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Usage" make
      |> Jsont.Object.opt_mem "input_tokens" Jsont.int ~enc:input_tokens
      |> Jsont.Object.opt_mem "output_tokens" Jsont.int ~enc:output_tokens
      |> Jsont.Object.opt_mem "total_tokens" Jsont.int ~enc:total_tokens
      |> Jsont.Object.opt_mem "cache_creation_input_tokens" Jsont.int ~enc:cache_creation_input_tokens
      |> Jsont.Object.opt_mem "cache_read_input_tokens" Jsont.int ~enc:cache_read_input_tokens
      |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
      |> Jsont.Object.finish

    let effective_input_tokens t =
      match t.input_tokens with
      | None -> 0
      | Some input ->
          let cached = Option.value t.cache_read_input_tokens ~default:0 in
          max 0 (input - cached)

    let total_cost_estimate t ~input_price ~output_price =
      match t.input_tokens, t.output_tokens with
      | Some input, Some output ->
          let input_cost = float_of_int input *. input_price /. 1_000_000. in
          let output_cost = float_of_int output *. output_price /. 1_000_000. in
          Some (input_cost +. output_cost)
      | _ -> None

    let pp fmt t =
      Fmt.pf fmt "@[<2>Usage@ { input = %a;@ output = %a;@ total = %a;@ \
                  cache_creation = %a;@ cache_read = %a }@]"
        Fmt.(option int) t.input_tokens
        Fmt.(option int) t.output_tokens
        Fmt.(option int) t.total_tokens
        Fmt.(option int) t.cache_creation_input_tokens
        Fmt.(option int) t.cache_read_input_tokens

    let to_json t =
      match Jsont.Json.encode jsont t with
      | Ok json -> json
      | Error msg -> failwith ("Usage.to_json: " ^ msg)

    let of_json json =
      match Jsont.Json.decode jsont json with
      | Ok v -> v
      | Error msg -> raise (Invalid_argument ("Usage.of_json: " ^ msg))
  end

  type t = {
    subtype : string;
    duration_ms : int;
    duration_api_ms : int;
    is_error : bool;
    num_turns : int;
    session_id : string;
    total_cost_usd : float option;
    usage : Usage.t option;
    result : string option;
    structured_output : Jsont.json option;
    unknown : Unknown.t;
  }

  let create ~subtype ~duration_ms ~duration_api_ms ~is_error ~num_turns
      ~session_id ?total_cost_usd ?usage ?result ?structured_output () =
    { subtype; duration_ms; duration_api_ms; is_error; num_turns;
      session_id; total_cost_usd; usage; result; structured_output; unknown = Unknown.empty }

  let make subtype duration_ms duration_api_ms is_error num_turns
      session_id total_cost_usd usage result structured_output unknown =
    { subtype; duration_ms; duration_api_ms; is_error; num_turns;
      session_id; total_cost_usd; usage; result; structured_output; unknown }

  let subtype t = t.subtype
  let duration_ms t = t.duration_ms
  let duration_api_ms t = t.duration_api_ms
  let is_error t = t.is_error
  let num_turns t = t.num_turns
  let session_id t = t.session_id
  let total_cost_usd t = t.total_cost_usd
  let usage t = t.usage
  let result t = t.result
  let structured_output t = t.structured_output
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"Result" make
    |> Jsont.Object.mem "subtype" Jsont.string ~enc:subtype
    |> Jsont.Object.mem "duration_ms" Jsont.int ~enc:duration_ms
    |> Jsont.Object.mem "duration_api_ms" Jsont.int ~enc:duration_api_ms
    |> Jsont.Object.mem "is_error" Jsont.bool ~enc:is_error
    |> Jsont.Object.mem "num_turns" Jsont.int ~enc:num_turns
    |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
    |> Jsont.Object.opt_mem "total_cost_usd" Jsont.number ~enc:total_cost_usd
    |> Jsont.Object.opt_mem "usage" Usage.jsont ~enc:usage
    |> Jsont.Object.opt_mem "result" Jsont.string ~enc:result
    |> Jsont.Object.opt_mem "structured_output" Jsont.json ~enc:structured_output
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let to_json t =
    let fields = [
      (Jsont.Json.name "type", Jsont.String ("result", Jsont.Meta.none));
      (Jsont.Json.name "subtype", Jsont.String (t.subtype, Jsont.Meta.none));
      (Jsont.Json.name "duration_ms", Jsont.Number (float_of_int t.duration_ms, Jsont.Meta.none));
      (Jsont.Json.name "duration_api_ms", Jsont.Number (float_of_int t.duration_api_ms, Jsont.Meta.none));
      (Jsont.Json.name "is_error", Jsont.Bool (t.is_error, Jsont.Meta.none));
      (Jsont.Json.name "num_turns", Jsont.Number (float_of_int t.num_turns, Jsont.Meta.none));
      (Jsont.Json.name "session_id", Jsont.String (t.session_id, Jsont.Meta.none));
    ] in
    let fields = match t.total_cost_usd with
      | Some cost -> (Jsont.Json.name "total_cost_usd", Jsont.Number (cost, Jsont.Meta.none)) :: fields
      | None -> fields
    in
    let fields = match t.usage with
      | Some usage -> (Jsont.Json.name "usage", Usage.to_json usage) :: fields
      | None -> fields
    in
    let fields = match t.result with
      | Some result -> (Jsont.Json.name "result", Jsont.String (result, Jsont.Meta.none)) :: fields
      | None -> fields
    in
    let fields = match t.structured_output with
      | Some output -> (Jsont.Json.name "structured_output", output) :: fields
      | None -> fields
    in
    Jsont.Object (fields, Jsont.Meta.none)

  let of_json json =
    match Jsont.Json.decode jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("Result.of_json: " ^ msg))

  let pp fmt t =
    if t.is_error then
      Fmt.pf fmt "@[<2>Result.error@ { session = %S;@ result = %a }@]"
        t.session_id
        Fmt.(option string) t.result
    else
      let tokens_info = match t.usage with
        | Some u ->
            let input = Usage.input_tokens u in
            let output = Usage.output_tokens u in
            let cached = Usage.cache_read_input_tokens u in
            (match input, output, cached with
            | Some i, Some o, Some c when c > 0 ->
                Printf.sprintf " (tokens: %d+%d, cached: %d)" i o c
            | Some i, Some o, _ ->
                Printf.sprintf " (tokens: %d+%d)" i o
            | _ -> "")
        | None -> ""
      in
      Fmt.pf fmt "@[<2>Result.%s@ { duration = %dms;@ cost = $%.4f%s }@]"
        t.subtype
        t.duration_ms
        (Option.value t.total_cost_usd ~default:0.0)
        tokens_info
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | System of System.t
  | Result of Result.t

let user_string s = User (User.create_string s)
let user_blocks blocks = User (User.create_blocks blocks)
let user_with_tool_result ~tool_use_id ~content ?is_error () =
  User (User.create_with_tool_result ~tool_use_id ~content ?is_error ())

let assistant ~content ~model ?error () = Assistant (Assistant.create ~content ~model ?error ())
let assistant_text ~text ~model ?error () =
  Assistant (Assistant.create ~content:[Content_block.text text] ~model ?error ())

let system_init ~session_id =
  System (System.init ~session_id ())
let system_error ~error =
  System (System.error ~error)

let result ~subtype ~duration_ms ~duration_api_ms ~is_error ~num_turns
    ~session_id ?total_cost_usd ?usage ?result ?structured_output () =
  Result (Result.create ~subtype ~duration_ms ~duration_api_ms ~is_error
    ~num_turns ~session_id ?total_cost_usd ?usage ?result ?structured_output ())

let to_json = function
  | User t -> User.to_json t
  | Assistant t -> Assistant.to_json t
  | System t -> System.to_json t
  | Result t -> Result.to_json t

(* Jsont codec for the main Message variant type.
   Uses case_mem for discriminated union based on "type" field. *)
let jsont : t Jsont.t =
  let case_map kind obj dec = Jsont.Object.Case.map kind obj ~dec in
  let case_user = case_map "user" User.incoming_jsont (fun v -> User v) in
  let case_assistant = case_map "assistant" Assistant.incoming_jsont (fun v -> Assistant v) in
  let case_system = case_map "system" System.jsont (fun v -> System v) in
  let case_result = case_map "result" Result.jsont (fun v -> Result v) in
  let enc_case = function
    | User v -> Jsont.Object.Case.value case_user v
    | Assistant v -> Jsont.Object.Case.value case_assistant v
    | System v -> Jsont.Object.Case.value case_system v
    | Result v -> Jsont.Object.Case.value case_result v
  in
  let cases = Jsont.Object.Case.[
    make case_user;
    make case_assistant;
    make case_system;
    make case_result
  ] in
  Jsont.Object.map ~kind:"Message" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
      ~tag_to_string:Fun.id ~tag_compare:String.compare
  |> Jsont.Object.finish

let of_json json =
  match Jsont.Json.decode jsont json with
  | Ok v -> v
  | Error msg -> raise (Invalid_argument ("Message.of_json: " ^ msg))

let pp fmt = function
  | User t -> User.pp fmt t
  | Assistant t -> Assistant.pp fmt t
  | System t -> System.pp fmt t
  | Result t -> Result.pp fmt t

let is_user = function User _ -> true | _ -> false
let is_assistant = function Assistant _ -> true | _ -> false
let is_system = function System _ -> true | _ -> false
let is_result = function Result _ -> true | _ -> false

let is_error = function
  | Result r -> Result.is_error r
  | System s -> System.subtype s = "error"
  | _ -> false

let extract_text = function
  | User u -> User.as_text u
  | Assistant a -> 
      let text = Assistant.combined_text a in
      if text = "" then None else Some text
  | _ -> None

let extract_tool_uses = function
  | Assistant a -> Assistant.get_tool_uses a
  | _ -> []

let get_session_id = function
  | System s -> System.session_id s
  | Result r -> Some (Result.session_id r)
  | _ -> None

let log_received t =
  Log.info (fun m -> m "← %a" pp t)

let log_sending t =
  Log.info (fun m -> m "→ %a" pp t)

let log_error msg t =
  Log.err (fun m -> m "%s: %a" msg pp t)

