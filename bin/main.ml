open Cohttp_lwt_unix

let counter = ref 0

let img_res () =
  let svg =
    {|
      <svg width='191' height='100' xmlns='http://www.w3.org/2000/svg'>
        <rect width='100%' height='100%' fill='black'/>
        <text 
          x='95.5' 
          y='40' 
          text-anchor='middle' 
          dominant-baseline='middle' 
          font-family='Arial' 
          font-size='12' 
          fill='white'>
          This Frame is Written in OCaml
        </text>
        <text 
          x='95.5' 
          y='60' 
          text-anchor='middle' 
          dominant-baseline='middle' 
          font-family='Arial' 
          font-size='12' 
          fill='white'>
          counter: |} ^ string_of_int !counter
    ^ {|</text>
      </svg>
    |}
  in
  let headers = Cohttp.Header.init_with "Content-Type" "image/svg+xml" in
  let headers = Cohttp.Header.add headers "Cache-Control" "no-cache" in
  Server.respond_string ~status:`OK ~body:svg ~headers ()
;;

type button = { content : string }

type frame =
  { title : string
  ; image : string
  ; buttons : button list
  ; post_url : string
  }

type action = { button_index : int [@key "buttonIndex"] }
[@@deriving yojson { strict = false }]

type post = { untrusted_data : action [@key "untrustedData"] }
[@@deriving yojson { strict = false }]

let render button index =
  Printf.sprintf "<meta property='fc:frame:button:%i' content='%s'/>" index button.content
;;

let render frame =
  let buttons = List.mapi (fun i btn -> i + 1, btn) frame.buttons in
  let buttons = List.fold_left (fun acc (i, btn) -> acc ^ render btn i) "" buttons in
  Printf.sprintf
    {|
      <!DOCTYPE html>
      <html>
        <head>
          <meta property='og:title' content='%s'/>
          <meta property='og:image' content='%s' />
	  		  <meta property='fc:frame' content='vNext' />
	  		  <meta property='fc:frame:image' content='%s'/>
          <meta property='fc:frame:post_url' content='%s' />
          %s
        </head>
      </html>
    |}
    frame.title
    frame.image
    (frame.image ^ "/" ^ (string_of_float (Unix.time ())))
    frame.post_url
    buttons
;;

let frame () =
  let frame =
    { title = "Simple Frame"
    ; image = "https://da23-12-170-106-2.ngrok-free.app/image"
    ; buttons = [ { content = "+" }; { content = "-" } ]
    ; post_url = "https://da23-12-170-106-2.ngrok-free.app/post"
    }
  in
  let html = render frame in
  let headers = Cohttp.Header.init () in
  let headers = Cohttp.Header.add headers "Cache-Control" "no-cache" in
  Server.respond_string ~status:`OK ~body:html ~headers ()
;;

let handle_post body =
  let open Lwt.Syntax in
  let () = print_string "in post" in
  let* body = Cohttp_lwt.Body.to_string body in
  let json = Yojson.Safe.from_string body in
  let action =
    match post_of_yojson json with
    | Ok post -> post.untrusted_data
    | Error e ->
      let () = print_string e in
      raise (Failure e)
  in
  let () =
    if action.button_index = 1
    then (
      let () = print_string "inc" in
      counter := !counter + 1)
    else counter := !counter - 1
  in
  frame ()
;;

let handler _con req body =
  let uri = req |> Request.uri |> Uri.path in 
  let uri = List.tl (String.split_on_char '/' uri) in
  match uri with
  | ["frame"] -> frame ()
  | ["image"; _] -> img_res ()
  | ["post"] -> handle_post body
  | _ -> Server.respond_string ~status:`OK ~body:"not found" ()
;;

let start_server =
  let open Lwt.Syntax in
  let server = Server.make ~callback:handler () in
  let* ctx = Conduit_lwt_unix.init ~src:"0.0.0.0" () in
  let ctx = Cohttp_lwt_unix.Client.custom_ctx ~ctx () in
  Server.create ~ctx ~mode:(`TCP (`Port 443)) server
;;

Lwt_main.run start_server
