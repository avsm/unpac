(** Event types emitted by the Claude agent for live UI updates. *)

type tool_call = {
  id : string;
  name : string;
  input : string;  (* JSON string *)
}

type tool_result = {
  id : string;
  name : string;
  output : string;
  is_error : bool;
}

type t =
  | Thinking
  | Text of string
  | Tool_call of tool_call
  | Tool_result of tool_result
  | Error of string
  | Sync of string  (* "status" or "push" *)
  | Turn_complete of { turn : int; cost_usd : float option }
  | Agent_start
  | Agent_stop

(* Simple JSON string escaping *)
let escape_json_string s =
  let buf = Buffer.create (String.length s + 16) in
  Buffer.add_char buf '"';
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 32 ->
        Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let to_json = function
  | Thinking ->
      {|{"type":"thinking"}|}
  | Text s ->
      Printf.sprintf {|{"type":"text","content":%s}|}
        (escape_json_string s)
  | Tool_call { id; name; input } ->
      Printf.sprintf {|{"type":"tool_call","id":%s,"name":%s,"input":%s}|}
        (escape_json_string id)
        (escape_json_string name)
        input  (* input is already JSON *)
  | Tool_result { id; name; output; is_error } ->
      Printf.sprintf {|{"type":"tool_result","id":%s,"name":%s,"output":%s,"is_error":%b}|}
        (escape_json_string id)
        (escape_json_string name)
        (escape_json_string output)
        is_error
  | Error msg ->
      Printf.sprintf {|{"type":"error","message":%s}|}
        (escape_json_string msg)
  | Sync action ->
      Printf.sprintf {|{"type":"sync","action":%s}|}
        (escape_json_string action)
  | Turn_complete { turn; cost_usd } ->
      let cost_str = match cost_usd with
        | Some c -> Printf.sprintf "%.6f" c
        | None -> "null"
      in
      Printf.sprintf {|{"type":"turn_complete","turn":%d,"cost_usd":%s}|}
        turn cost_str
  | Agent_start ->
      {|{"type":"agent_start"}|}
  | Agent_stop ->
      {|{"type":"agent_stop"}|}

(* Event bus for broadcasting to listeners *)
type listener = t -> unit

type bus = {
  mutable listeners : listener list;
}

let create_bus () = { listeners = [] }

let subscribe bus listener =
  bus.listeners <- listener :: bus.listeners

let unsubscribe bus listener =
  bus.listeners <- List.filter (fun l -> l != listener) bus.listeners

let emit bus event =
  List.iter (fun l -> l event) bus.listeners
