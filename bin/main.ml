open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type message =
  { role : string
  ; content : string
  ; refusal : string option
  }
[@@deriving yojson]

type top_logprob =
  { token : string
  ; logprob : float
  ; bytes : int list option
  }
[@@deriving yojson]

type content =
  { token : string
  ; logprob : float
  ; bytes : int list option
  ; top_logprobs : top_logprob list
  }
[@@deriving yojson]

type logprobs = { content : content list option } [@@deriving yojson]

type choice =
  { finish_reason : string
  ; index : int
  ; message : message
  ; logprobs : logprobs option
  }
[@@deriving yojson]

type usage =
  { completion_tokens : int
  ; prompt_tokens : int
  ; total_tokens : int
  }
[@@deriving yojson]

type response =
  { id : string
  ; choices : choice list
  ; created : int
  ; model : string
  ; service_tier : string option [@yojson.option]
  ; system_fingerprint : string
  ; obj : string [@key "object"]
  ; usage : usage
  }
[@@deriving yojson]

type message_log_item =
  { role : string
  ; content : string
  }
[@@deriving yojson]

type message_log = message_log_item list [@@deriving yojson]

let debug = ref false

let log_file_path =
  let home_path =
    match Sys.getenv_opt "HOME" with
    | Some x -> x
    | None -> failwith "Unable to find HOME"
  in
  Filename.concat home_path "jippity_last_convo.json"
;;

let parse_top_choice resp =
  let resp = Yojson.Safe.from_string resp in
  let resp = response_of_yojson resp in
  match resp.choices with
  | h :: _ -> h.message.content
  | [] -> failwith "Invalid response. No choices found."
;;

(* LMAO *)
let null_auth ?ip:_ ~host:_ _ = Ok None

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw
;;

let parse_args () =
  let prompt = ref "" in
  let sys_msg = ref "" in
  let continue = ref false in
  let speclist =
    [ "-s", Arg.Set_string sys_msg, "Custom system message"
    ; "-c", Arg.Set continue, "Continue the last conversation"
    ; "-d", Arg.Set debug, "Print debug info"
    ]
  in
  let usage_msg = {|Usage: jippity [-s "<system_message>"] [-c] "<prompt>"|} in
  Arg.parse speclist (fun p -> prompt := String.trim p) usage_msg;
  (* Check for arg errors *)
  let prompt =
    match !prompt with
    | "" -> invalid_arg "Missing prompt."
    | x -> x
  in
  let sys_msg = String.trim !sys_msg in
  let continue = !continue in
  if sys_msg <> "" && continue
  then invalid_arg "Cannot specify system message in a continued conversation.";
  let sys_msgs =
    match sys_msg with
    | "" -> []
    | x -> [ { role = "system"; content = x } ]
  in
  prompt, sys_msgs, continue
;;

let read_api_key () =
  Dotenv.export ();
  let api_key = Sys.getenv_opt "OPENAI_API_KEY" in
  match api_key with
  | Some x -> x
  | None -> failwith "OPENAI_API_KEY not found in environment variables."
;;

let print_boxed text =
  let len = String.length text in
  let border = String.make (len + 4) '-' in
  Printf.printf "+%s+\n" border;
  Printf.printf "|  %s  |\n" text;
  Printf.printf "+%s+\n" border
;;

let init_msg_log prompt sys_msgs continue =
  let open_flags = [ Open_rdonly; Open_creat; Open_text ] in
  let open_flags = if continue then open_flags else Open_trunc :: open_flags in
  let read_log_file default ic =
    (match In_channel.input_all ic with
     | "" -> Yojson.Safe.to_string (yojson_of_message_log default)
     | x -> x)
    |> Yojson.Safe.from_string
    |> message_log_of_yojson
  in
  In_channel.with_open_gen open_flags 0o644 log_file_path (read_log_file sys_msgs)
  @ [ { role = "user"; content = prompt } ]
;;

let send_req client sw msg_log api_key =
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let headers = Cohttp.Header.add headers "Authorization" ("Bearer " ^ api_key) in
  let req =
    Printf.sprintf
      {|{ "model": "gpt-4o-mini", "messages": %s }|}
      (Yojson.Safe.to_string (yojson_of_message_log msg_log))
  in
  let body = Eio.Flow.string_source req in
  let resp, body =
    Cohttp_eio.Client.post
      ~sw
      ~headers
      ~body
      client
      (Uri.of_string "https://api.openai.com/v1/chat/completions")
  in
  if Http.Status.compare resp.status `OK <> 0
  then Fmt.epr "Unexpected HTTP status: %a" Http.Status.pp resp.status;
  Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
;;

let write_msg_log msg_log =
  let open_flags = [ Open_wronly; Open_trunc; Open_creat; Open_text ] in
  Out_channel.with_open_gen open_flags 0o644 log_file_path (fun oc ->
    Out_channel.output_string oc (Yojson.Safe.to_string (yojson_of_message_log msg_log)))
;;

let () =
  let prompt, sys_msgs, continue = parse_args () in
  let api_key = read_api_key () in
  let msg_log = init_msg_log prompt sys_msgs continue in
  Eio_main.run
  @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env
  @@ fun () ->
  Eio.Switch.run
  @@ fun sw ->
  let client =
    Cohttp_eio.Client.make ~https:(Some (https ~authenticator:null_auth)) env#net
  in
  let resp_body = send_req client sw msg_log api_key in
  if !debug
  then (
    print_boxed "Debug info:";
    print_endline resp_body);
  let reply_msg = parse_top_choice resp_body in
  print_boxed "Response:";
  print_endline reply_msg;
  write_msg_log (msg_log @ [ { role = "assistant"; content = reply_msg } ])
;;
