open Eio
open Piaf

module Route = struct
  module Path = struct
    open Routes

    type path = Home | Wiki of { page : string } | Styles

    let home () = nil
    let wiki () = s "wiki" / str /? nil
    let styles () = s "styles" / s "base.css" /? nil

    let router =
      Routes.(
        one_of
          [
            route (home ()) Home;
            route (wiki ()) (fun page -> Wiki { page });
            route (styles ()) Styles;
          ])

    let _to_string path =
      match path with
      | Home -> sprintf (home ())
      | Wiki { page } -> sprintf (wiki ()) page
      | Styles -> sprintf (styles ())

    let of_string string =
      match match' router ~target:string with
      | NoMatch -> None
      (* TODO: trailing? *)
      (* TODO: better than not found error message when trailing *)
      | MatchWithTrailingSlash _path -> None
      | FullMatch path -> Some path
  end
end

let invalid_method () =
  let body = Body.of_string "Invalid method" in
  Response.create ~body `Method_not_allowed

let server_error exn =
  let message = Format.sprintf "Server error: %s" (Printexc.to_string exn) in
  let body = Body.of_string message in
  Response.create ~body `Internal_server_error

(* TODO: better way of handling *)
exception Invalid_markdown

let extract_title ~markdown =
  let open Omd in
  let block : attributes block =
    match markdown with [ block ] -> block | _ -> raise Invalid_markdown
  in
  let size, inline =
    match block with
    (* TODO: explain inline *)
    | Heading (_attr, size, inline) -> (size, inline)
    | Paragraph (_, _)
    | List (_, _, _, _)
    | Blockquote (_, _)
    | Thematic_break _
    | Code_block (_, _, _)
    | Html_block (_, _)
    | Definition_list (_, _)
    | Table (_, _, _) ->
        raise Invalid_markdown
  in
  let () = match size with 1 -> () | _ -> raise Invalid_markdown in
  match inline with
  | Text (_, content) -> content
  | Concat (_, _)
  | Emph (_, _)
  | Strong (_, _)
  | Code (_, _)
  | Hard_break _ | Soft_break _
  | Link (_, _)
  | Image (_, _)
  | Html (_, _) ->
      raise Invalid_markdown

let render ~title ~content =
  (* TODO: better render function *)
  Format.sprintf
    {|
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta
      name="viewport"
      content="width=device-width, initial-scale=1, maximum-scale=1"
    />
    <link rel="stylesheet" href="/styles/base.css" />

    <title>%s</title>
  </head>
  <body>
    %s
  </body>
</html>
  |}
    title content

let not_found ~cwd =
  let markdown = Omd.of_string @@ Path.(load @@ (cwd / "not_found.md")) in
  let content = Omd.to_html markdown in
  (* TODO: better than 404? Maybe link to create page *)
  let body = Body.of_string @@ render ~title:"404 - Doskya" ~content in
  Response.create ~body `Not_found

let load_if_fail_not_found ~cwd ~path k =
  try k @@ Path.load path with _exn -> not_found ~cwd
(* TODO: ensure exception is the right one *)

(* TODO: markdown currently supports HTML *)
let on_home ~cwd =
  let markdown = Omd.of_string @@ Path.(load @@ (cwd / "home.md")) in
  let content = Omd.to_html markdown in
  let body = Body.of_string @@ render ~title:"Doskya" ~content in
  (* TODO: content-type *)
  Response.create ~body `OK

let on_wiki ~cwd ~page =
  let path = Path.(cwd / "wiki" / (page ^ ".md")) in
  load_if_fail_not_found ~cwd ~path @@ fun markdown ->
  let markdown = Omd.of_string markdown in
  let title = extract_title ~markdown ^ " - Doskya" in
  let content = Omd.to_html markdown in
  let body = Body.of_string @@ render ~title ~content in
  (* TODO: content-type *)
  Response.create ~body `OK

let on_styles ~cwd =
  let base_css = Path.(load @@ (cwd / "styles" / "base.css")) in
  let body = Body.of_string base_css in
  (* TODO: content-type *)
  Response.create ~body `OK

let main () =
  (* let () = Logs_threaded.enable () in *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  let config =
    let interface = Eio.Net.Ipaddr.V4.any in
    let port = 3000 in
    Piaf.Server.Config.create (`Tcp (interface, port))
  in
  let server =
    Server.create ~config @@ fun { ctx = _; request } ->
    try
      (* TODO: use ctx *)
      (* TODO: headers? Maybe content-type? *)
      (* TODO: body? Drain? *)
      let Request.
            { meth; target; version = _; headers = _; scheme = _; body = _ } =
        request
      in
      match meth with
      | `GET -> (
          match Route.Path.of_string target with
          | Some Home -> on_home ~cwd
          | Some (Wiki { page }) -> on_wiki ~cwd ~page
          | Some Styles -> on_styles ~cwd
          | None -> not_found ~cwd)
      | `Other _ | `PUT | `OPTIONS | `CONNECT | `TRACE | `DELETE | `HEAD | `POST
        ->
          invalid_method ()
    with exn -> server_error exn
  in
  let _command = Server.Command.start ~sw env server in
  ()

let () = main ()
