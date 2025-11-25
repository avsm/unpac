let src = Logs.Src.create "claude.content_block" ~doc:"Claude content blocks"

module Log = (val Logs.src_log src : Logs.LOG)

module Text = struct
  type t = { text : string; unknown : Unknown.t }

  let create text = { text; unknown = Unknown.empty }
  let make text unknown = { text; unknown }
  let text t = t.text
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"Text" make
    |> Jsont.Object.mem "text" Jsont.string ~enc:text
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
end

module Tool_use = struct
  module Input = struct
    (* Dynamic JSON data for tool inputs with typed accessors using jsont decoders *)
    type t = Jsont.json

    let jsont = Jsont.json

    let of_string_pairs pairs =
      Jsont.Json.object'
        (List.map
           (fun (k, v) ->
             Jsont.Json.mem (Jsont.Json.name k) (Jsont.Json.string v))
           pairs)

    let of_assoc (assoc : (string * Jsont.json) list) : t =
      Jsont.Json.object'
        (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) assoc)

    (* Helper to decode an optional field with a given codec *)
    let get_opt (type a) (codec : a Jsont.t) t key : a option =
      let field_codec =
        Jsont.Object.map ~kind:"field" (fun v -> v)
        |> Jsont.Object.opt_mem key codec ~enc:Fun.id
        |> Jsont.Object.finish
      in
      match Jsont.Json.decode field_codec t with Ok v -> v | Error _ -> None

    let get_string t key = get_opt Jsont.string t key
    let get_int t key = get_opt Jsont.int t key
    let get_bool t key = get_opt Jsont.bool t key
    let get_float t key = get_opt Jsont.number t key

    let keys t =
      (* Decode as object with all members captured as unknown *)
      match t with
      | Jsont.Object (members, _) ->
          List.map (fun ((name, _), _) -> name) members
      | _ -> []
  end

  type t = { id : string; name : string; input : Input.t; unknown : Unknown.t }

  let create ~id ~name ~input = { id; name; input; unknown = Unknown.empty }
  let make id name input unknown = { id; name; input; unknown }
  let id t = t.id
  let name t = t.name
  let input t = t.input
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"Tool_use" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "input" Input.jsont ~enc:input
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
end

module Tool_result = struct
  type t = {
    tool_use_id : string;
    content : string option;
    is_error : bool option;
    unknown : Unknown.t;
  }

  let create ~tool_use_id ?content ?is_error () =
    { tool_use_id; content; is_error; unknown = Unknown.empty }

  let make tool_use_id content is_error unknown =
    { tool_use_id; content; is_error; unknown }

  let tool_use_id t = t.tool_use_id
  let content t = t.content
  let is_error t = t.is_error
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"Tool_result" make
    |> Jsont.Object.mem "tool_use_id" Jsont.string ~enc:tool_use_id
    |> Jsont.Object.opt_mem "content" Jsont.string ~enc:content
    |> Jsont.Object.opt_mem "is_error" Jsont.bool ~enc:is_error
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
end

module Thinking = struct
  type t = { thinking : string; signature : string; unknown : Unknown.t }

  let create ~thinking ~signature =
    { thinking; signature; unknown = Unknown.empty }

  let make thinking signature unknown = { thinking; signature; unknown }
  let thinking t = t.thinking
  let signature t = t.signature
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"Thinking" make
    |> Jsont.Object.mem "thinking" Jsont.string ~enc:thinking
    |> Jsont.Object.mem "signature" Jsont.string ~enc:signature
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
end

type t =
  | Text of Text.t
  | Tool_use of Tool_use.t
  | Tool_result of Tool_result.t
  | Thinking of Thinking.t

let text s = Text (Text.create s)
let tool_use ~id ~name ~input = Tool_use (Tool_use.create ~id ~name ~input)

let tool_result ~tool_use_id ?content ?is_error () =
  Tool_result (Tool_result.create ~tool_use_id ?content ?is_error ())

let thinking ~thinking ~signature =
  Thinking (Thinking.create ~thinking ~signature)

let jsont : t Jsont.t =
  let case_map kind obj dec = Jsont.Object.Case.map kind obj ~dec in

  let case_text = case_map "text" Text.jsont (fun v -> Text v) in
  let case_tool_use =
    case_map "tool_use" Tool_use.jsont (fun v -> Tool_use v)
  in
  let case_tool_result =
    case_map "tool_result" Tool_result.jsont (fun v -> Tool_result v)
  in
  let case_thinking =
    case_map "thinking" Thinking.jsont (fun v -> Thinking v)
  in

  let enc_case = function
    | Text v -> Jsont.Object.Case.value case_text v
    | Tool_use v -> Jsont.Object.Case.value case_tool_use v
    | Tool_result v -> Jsont.Object.Case.value case_tool_result v
    | Thinking v -> Jsont.Object.Case.value case_thinking v
  in

  let cases =
    Jsont.Object.Case.
      [
        make case_text;
        make case_tool_use;
        make case_tool_result;
        make case_thinking;
      ]
  in

  Jsont.Object.map ~kind:"Content_block" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
       ~tag_to_string:Fun.id ~tag_compare:String.compare
  |> Jsont.Object.finish

let log_received t =
  Log.debug (fun m ->
      m "Received content block: %a" (Jsont.pp_value jsont ()) t)

let log_sending t =
  Log.debug (fun m -> m "Sending content block: %a" (Jsont.pp_value jsont ()) t)
