(** Minimal WebSocket server for live agent UI.

    Uses cohttp-eio for the HTTP upgrade handshake,
    then raw Eio sockets for WebSocket frames. *)

let src = Logs.Src.create "unpac.claude.web" ~doc:"Web server"
module Log = (val Logs.src_log src : Logs.LOG)

(* WebSocket frame helpers *)
module Ws = struct
  (* Compute Sec-WebSocket-Accept from client key *)
  let accept_key client_key =
    let magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
    let combined = client_key ^ magic in
    let hash = Digestif.SHA1.digest_string combined in
    Base64.encode_exn (Digestif.SHA1.to_raw_string hash)

  (* Send a WebSocket text frame *)
  let send_text flow text =
    let len = String.length text in
    let header =
      if len < 126 then
        let h = Bytes.create 2 in
        Bytes.set_uint8 h 0 0x81;  (* FIN + text opcode *)
        Bytes.set_uint8 h 1 len;
        Bytes.to_string h
      else if len < 65536 then
        let h = Bytes.create 4 in
        Bytes.set_uint8 h 0 0x81;
        Bytes.set_uint8 h 1 126;
        Bytes.set_uint16_be h 2 len;
        Bytes.to_string h
      else
        let h = Bytes.create 10 in
        Bytes.set_uint8 h 0 0x81;
        Bytes.set_uint8 h 1 127;
        Bytes.set_int64_be h 2 (Int64.of_int len);
        Bytes.to_string h
    in
    Eio.Flow.copy_string (header ^ text) flow

  (* Send close frame *)
  let send_close flow =
    let frame = Bytes.create 2 in
    Bytes.set_uint8 frame 0 0x88;  (* FIN + close opcode *)
    Bytes.set_uint8 frame 1 0;
    Eio.Flow.copy_string (Bytes.to_string frame) flow

  (* Read a WebSocket frame, returns (opcode, payload) or None on close *)
  let read_frame flow =
    let buf = Cstruct.create 2 in
    match Eio.Flow.read_exact flow buf with
    | exception End_of_file -> None
    | () ->
        let b0 = Cstruct.get_uint8 buf 0 in
        let b1 = Cstruct.get_uint8 buf 1 in
        let _fin = (b0 land 0x80) <> 0 in
        let opcode = b0 land 0x0F in
        let masked = (b1 land 0x80) <> 0 in
        let len0 = b1 land 0x7F in

        (* Get actual length *)
        let len =
          if len0 < 126 then len0
          else if len0 = 126 then begin
            let buf = Cstruct.create 2 in
            Eio.Flow.read_exact flow buf;
            Cstruct.BE.get_uint16 buf 0
          end else begin
            let buf = Cstruct.create 8 in
            Eio.Flow.read_exact flow buf;
            Int64.to_int (Cstruct.BE.get_uint64 buf 0)
          end
        in

        (* Get mask if present *)
        let mask =
          if masked then begin
            let buf = Cstruct.create 4 in
            Eio.Flow.read_exact flow buf;
            Some (Cstruct.to_bytes buf)
          end else None
        in

        (* Read payload *)
        let payload = Cstruct.create len in
        if len > 0 then Eio.Flow.read_exact flow payload;

        (* Unmask if needed *)
        let data =
          match mask with
          | None -> Cstruct.to_string payload
          | Some m ->
              let bytes = Cstruct.to_bytes payload in
              for i = 0 to len - 1 do
                let b = Bytes.get_uint8 bytes i in
                let k = Bytes.get_uint8 m (i mod 4) in
                Bytes.set_uint8 bytes i (b lxor k)
              done;
              Bytes.to_string bytes
        in
        Some (opcode, data)
end

(* Static HTML page *)
let index_html = {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>unpac-claude</title>
  <style>
    :root {
      --bg: #0d1117;
      --fg: #c9d1d9;
      --dim: #6e7681;
      --border: #30363d;
      --accent: #58a6ff;
      --green: #3fb950;
      --red: #f85149;
      --yellow: #d29922;
    }
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, monospace;
      font-size: 14px;
      line-height: 1.5;
      background: var(--bg);
      color: var(--fg);
      height: 100vh;
      display: flex;
      flex-direction: column;
    }
    header {
      padding: 12px 16px;
      border-bottom: 1px solid var(--border);
      display: flex;
      align-items: center;
      gap: 12px;
    }
    header h1 {
      font-size: 16px;
      font-weight: 600;
    }
    .status {
      font-size: 12px;
      padding: 2px 8px;
      border-radius: 12px;
      background: var(--border);
    }
    .status.connected { background: var(--green); color: #000; }
    .status.error { background: var(--red); color: #fff; }
    #events {
      flex: 1;
      overflow-y: auto;
      padding: 16px;
    }
    .event {
      margin-bottom: 8px;
      padding: 8px 12px;
      border-radius: 6px;
      border-left: 3px solid var(--border);
    }
    .event.thinking { border-color: var(--yellow); color: var(--dim); }
    .event.text { border-color: var(--fg); white-space: pre-wrap; }
    .event.tool_call { border-color: var(--accent); }
    .event.tool_result { border-color: var(--green); }
    .event.tool_result.error { border-color: var(--red); }
    .event.error { border-color: var(--red); background: rgba(248,81,73,0.1); }
    .event.sync { border-color: var(--yellow); }
    .event.turn_complete { border-color: var(--dim); color: var(--dim); font-size: 12px; }
    .tool-name {
      color: var(--accent);
      font-weight: 600;
    }
    .tool-input, .tool-output {
      margin-top: 4px;
      padding: 8px;
      background: rgba(0,0,0,0.3);
      border-radius: 4px;
      font-size: 12px;
      max-height: 200px;
      overflow: auto;
      white-space: pre-wrap;
      word-break: break-all;
    }
    .cost { color: var(--dim); }
    footer {
      padding: 8px 16px;
      border-top: 1px solid var(--border);
      font-size: 12px;
      color: var(--dim);
    }
  </style>
</head>
<body>
  <header>
    <h1>unpac-claude</h1>
    <span id="status" class="status">connecting...</span>
  </header>
  <div id="events"></div>
  <footer>
    <span id="turn">Turn: 0</span> |
    <span id="cost">Cost: $0.00</span>
  </footer>
  <script>
    const events = document.getElementById('events');
    const status = document.getElementById('status');
    const turnEl = document.getElementById('turn');
    const costEl = document.getElementById('cost');
    let totalCost = 0;
    let currentTurn = 0;

    function connect() {
      const ws = new WebSocket(`ws://${location.host}/ws`);

      ws.onopen = () => {
        status.textContent = 'connected';
        status.className = 'status connected';
      };

      ws.onclose = () => {
        status.textContent = 'disconnected';
        status.className = 'status';
        setTimeout(connect, 2000);
      };

      ws.onerror = () => {
        status.textContent = 'error';
        status.className = 'status error';
      };

      ws.onmessage = (e) => {
        const data = JSON.parse(e.data);
        handleEvent(data);
      };
    }

    function handleEvent(e) {
      const div = document.createElement('div');
      div.className = 'event ' + e.type;

      switch (e.type) {
        case 'thinking':
          div.textContent = '⋯ thinking...';
          break;
        case 'text':
          div.textContent = e.content;
          break;
        case 'tool_call':
          div.innerHTML = `<span class="tool-name">${esc(e.name)}</span>` +
            `<div class="tool-input">${esc(formatJson(e.input))}</div>`;
          break;
        case 'tool_result':
          if (e.is_error) div.classList.add('error');
          div.innerHTML = `<span class="tool-name">${esc(e.name)}</span> ${e.is_error ? '✗' : '✓'}` +
            `<div class="tool-output">${esc(truncate(e.output, 2000))}</div>`;
          break;
        case 'error':
          div.textContent = '✗ ' + e.message;
          break;
        case 'sync':
          div.textContent = '↻ sync: ' + e.action;
          break;
        case 'turn_complete':
          currentTurn = e.turn;
          if (e.cost_usd) totalCost += e.cost_usd;
          turnEl.textContent = 'Turn: ' + currentTurn;
          costEl.textContent = 'Cost: $' + totalCost.toFixed(4);
          div.textContent = `Turn ${e.turn} complete` + (e.cost_usd ? ` ($${e.cost_usd.toFixed(4)})` : '');
          break;
        case 'agent_start':
          div.textContent = '▶ Agent started';
          div.style.borderColor = 'var(--green)';
          break;
        case 'agent_stop':
          div.textContent = '■ Agent stopped';
          div.style.borderColor = 'var(--red)';
          break;
        default:
          div.textContent = JSON.stringify(e);
      }

      events.appendChild(div);
      events.scrollTop = events.scrollHeight;
    }

    function esc(s) {
      return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
    }

    function truncate(s, n) {
      return s.length > n ? s.slice(0, n) + '...[truncated]' : s;
    }

    function formatJson(s) {
      try {
        return JSON.stringify(JSON.parse(s), null, 2);
      } catch {
        return s;
      }
    }

    connect();
  </script>
</body>
</html>|}

(* We don't track clients for WebSocket broadcasting in this simple implementation.
   Instead, each WebSocket connection runs in its own fiber and subscribes to events. *)

type t = unit

let create _event_bus = ()

(* Broadcast is now a no-op since each connection handles its own events *)
let broadcast _t _event = ()

(* Handle WebSocket connection - each connection subscribes to events directly *)
let handle_websocket event_bus (flow : _ Eio.Net.stream_socket) =
  let closed = ref false in
  Log.info (fun m -> m "WebSocket client connected");

  (* Subscribe to events and send them to this client *)
  let listener event =
    if not !closed then begin
      try
        let json = Event.to_json event in
        Ws.send_text flow json
      with _ ->
        closed := true
    end
  in
  Event.subscribe event_bus listener;

  (* Read loop - handle pings and close *)
  let rec loop () =
    match Ws.read_frame flow with
    | None ->
        closed := true
    | Some (0x8, _) -> (* Close *)
        Ws.send_close flow;
        closed := true
    | Some (0x9, data) -> (* Ping -> Pong *)
        let pong = Bytes.create (2 + String.length data) in
        Bytes.set_uint8 pong 0 0x8A;  (* FIN + pong *)
        Bytes.set_uint8 pong 1 (String.length data);
        Bytes.blit_string data 0 pong 2 (String.length data);
        Eio.Flow.copy_string (Bytes.to_string pong) flow;
        loop ()
    | Some _ ->
        loop ()
  in
  (try loop () with _ -> ());
  closed := true;
  Event.unsubscribe event_bus listener;
  Log.info (fun m -> m "WebSocket client disconnected")

(* Parse HTTP request headers *)
let parse_request data =
  let lines = String.split_on_char '\n' data in
  let headers = Hashtbl.create 16 in
  let path = ref "/" in
  List.iteri (fun i line ->
    let line = String.trim line in
    if i = 0 then begin
      (* Request line: GET /path HTTP/1.1 *)
      match String.split_on_char ' ' line with
      | _ :: p :: _ -> path := p
      | _ -> ()
    end else begin
      match String.index_opt line ':' with
      | Some idx ->
          let key = String.lowercase_ascii (String.trim (String.sub line 0 idx)) in
          let value = String.trim (String.sub line (idx + 1) (String.length line - idx - 1)) in
          Hashtbl.add headers key value
      | None -> ()
    end
  ) lines;
  (!path, headers)

(* Handle HTTP request *)
let handle_request event_bus (flow : _ Eio.Net.stream_socket) =
  let buf = Buffer.create 4096 in
  let chunk = Cstruct.create 4096 in

  (* Read request *)
  let rec read_headers () =
    match Eio.Flow.single_read flow chunk with
    | n ->
        Buffer.add_string buf (Cstruct.to_string (Cstruct.sub chunk 0 n));
        let data = Buffer.contents buf in
        if String.length data > 4 &&
           String.sub data (String.length data - 4) 4 = "\r\n\r\n"
        then data
        else read_headers ()
    | exception End_of_file -> Buffer.contents buf
  in
  let request = read_headers () in
  let (path, headers) = parse_request request in

  Log.debug (fun m -> m "Request: %s" path);

  (* Check for WebSocket upgrade *)
  let is_upgrade =
    Hashtbl.find_opt headers "upgrade" = Some "websocket" &&
    Hashtbl.mem headers "sec-websocket-key"
  in

  if path = "/ws" && is_upgrade then begin
    (* WebSocket handshake *)
    let key = Hashtbl.find headers "sec-websocket-key" in
    let accept = Ws.accept_key key in
    let response = Printf.sprintf
      "HTTP/1.1 101 Switching Protocols\r\n\
       Upgrade: websocket\r\n\
       Connection: Upgrade\r\n\
       Sec-WebSocket-Accept: %s\r\n\r\n" accept
    in
    Eio.Flow.copy_string response flow;
    handle_websocket event_bus flow
  end else begin
    (* Serve static content *)
    let (status, content_type, body) =
      if path = "/" || path = "/index.html" then
        ("200 OK", "text/html", index_html)
      else
        ("404 Not Found", "text/plain", "Not Found")
    in
    let response = Printf.sprintf
      "HTTP/1.1 %s\r\n\
       Content-Type: %s\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\r\n%s"
      status content_type (String.length body) body
    in
    Eio.Flow.copy_string response flow
  end

(* Start the web server *)
let start ~sw ~net ~port event_bus =
  let t = create event_bus in

  (* Listen for connections *)
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr in
  Log.info (fun m -> m "Web server listening on http://localhost:%d" port);

  (* Accept loop *)
  let rec accept_loop () =
    let flow, _addr = Eio.Net.accept ~sw socket in
    Eio.Fiber.fork ~sw (fun () ->
      try handle_request event_bus flow
      with exn ->
        Log.warn (fun m -> m "Request error: %s" (Printexc.to_string exn))
    );
    accept_loop ()
  in
  Eio.Fiber.fork ~sw accept_loop;
  t
