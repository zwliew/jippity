open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type message = { role : string; content : string } [@@deriving yojson]

type top_logprob = { token : string; logprob : float; bytes : int list option }
[@@deriving yojson]

type content = {
  token : string;
  logprob : float;
  bytes : int list option;
  top_logprobs : top_logprob list;
}
[@@deriving yojson]

type logprobs = { content : content list option } [@@deriving yojson]

type choice = {
  finish_reason : string;
  index : int;
  message : message;
  logprobs : logprobs option;
}
[@@deriving yojson]

type usage = {
  completion_tokens : int;
  prompt_tokens : int;
  total_tokens : int;
}
[@@deriving yojson]

type response = {
  id : string;
  choices : choice list;
  created : int;
  model : string;
  service_tier : string option; [@yojson.option]
  system_fingerprint : string;
  obj : string; [@key "object"]
  usage : usage;
}
[@@deriving yojson]

let parse_top_choice resp =
  let resp = Yojson.Safe.from_string resp in
  let resp = response_of_yojson resp in
  match resp.choices with
  | h :: _ -> h.message.content
  | [] -> failwith "Invalid response. No choices found."

let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let () =
  (* TODO: use the Arg module *)
  if Array.length Sys.argv <> 2 then failwith {|Usage: jippity "<prompt>"|};
  let prompt = Yojson.Safe.to_string @@ `String Sys.argv.(1) in

  (* TODO: Vet the dotenv library *)
  Dotenv.export ();
  let api_key =
    match Sys.getenv_opt "OPENAI_API_KEY" with
    | Some x -> x
    | None -> failwith "API_KEY not found in environment variables."
  in

  let req =
    Printf.sprintf
      {|{ "model": "gpt-4o-mini", "messages": [ { "role": "system", "content": "You are a helpful assistant." }, { "role": "user", "content": %s } ] }|}
      prompt
  in

  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let headers =
    Cohttp.Header.add headers "Authorization" ("Bearer " ^ api_key)
  in
  let body = Eio.Flow.string_source req in

  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client =
    Cohttp_eio.Client.make
      ~https:(Some (https ~authenticator:null_auth))
      env#net
  in

  Eio.Switch.run @@ fun sw ->
  let resp, body =
    Cohttp_eio.Client.post ~sw ~headers ~body client
      (Uri.of_string "https://api.openai.com/v1/chat/completions")
  in

  if Http.Status.compare resp.status `OK <> 0 then
    Fmt.epr "Unexpected HTTP status: %a" Http.Status.pp resp.status;

  let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
  print_endline @@ parse_top_choice body
