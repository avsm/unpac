let src = Logs.Src.create "claude.control" ~doc:"Claude control messages"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  request_id : string;
  subtype : string;
  data : Jsont.json;
  unknown : Unknown.t;
}

let jsont =
  Jsont.Object.map ~kind:"Control"
    (fun request_id subtype data unknown -> {request_id; subtype; data; unknown})
  |> Jsont.Object.mem "request_id" Jsont.string ~enc:(fun t -> t.request_id)
  |> Jsont.Object.mem "subtype" Jsont.string ~enc:(fun t -> t.subtype)
  |> Jsont.Object.mem "data" Jsont.json ~enc:(fun t -> t.data)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish

let create ~request_id ~subtype ~data =
  { request_id; subtype; data; unknown = Unknown.empty }

let request_id t = t.request_id
let subtype t = t.subtype
let data t = t.data

let to_json t =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify jsont t with
  | Ok s ->
      (match Jsont_bytesrw.decode_string' Jsont.json s with
      | Ok json -> json
      | Error e -> failwith (Jsont.Error.to_string e))
  | Error e -> failwith e

let of_json json =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json json with
  | Ok s ->
      (match Jsont_bytesrw.decode_string jsont s with
      | Ok t -> t
      | Error e -> raise (Invalid_argument ("Control.of_json: " ^ e)))
  | Error e -> raise (Invalid_argument ("Control.of_json: " ^ e))

let log_received t =
  Log.debug (fun m -> m "Received control message: %a" (Jsont.pp_value jsont ()) t)

let log_sending t =
  Log.debug (fun m -> m "Sending control message: %a" (Jsont.pp_value jsont ()) t)