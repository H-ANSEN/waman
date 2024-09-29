open Lwt.Syntax

(*<><> Extracting Resources From HTML <><><><><><><><><><><><><><><><><><><><>*)

let resolve base uri =
  Uri.of_string uri |> Uri.resolve "" base

let image_sources src html =
  Soup.(html |> select "img" 
             |> to_list 
             |> List.filter_map (attribute "src")
             |> List.map (resolve src))

let style_sheets src html =
  Soup.(html |> select "link[rel=stylesheet]" 
             |> to_list 
             |> List.filter_map (attribute "href")
             |> List.map (resolve src))

(*<><> Fetching Resource <><><><><><><><><><><><><><><><><><><><><><><><><><><*)

let get_resource uri =
  let* _, body = Cohttp_lwt_unix.Client.get uri in (* TO-DO handle response *)
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return_some body
;;

let timeout sec =
  let* () = Lwt_unix.sleep sec in
  Lwt.return_none
;;

let fetch uri time =
  Lwt.pick [get_resource uri; timeout time]
;;

(*<><> Downloading Resource <><><><><><><><><><><><><><><><><><><><><><><><><>*)
(* for now we hack together a directory containing the site contents          *)

let drop_scheme uri =
  let uri = Uri.with_scheme uri None |> Uri.to_string in
  if String.starts_with ~prefix:"//" uri then
    String.sub uri 2 (String.length uri - 2)
  else
    uri

let truncate name =
  let path = Filename.dirname name in
  let base = Filename.basename name in
  if String.length base > Fs.name_max then 
    let hash = Digest.string base |> Digest.to_hex in
    String.concat Filename.dir_sep [path; hash]
  else
    name

let store_source source =
  let source_str = source |> drop_scheme |> truncate in
  let path = Filename.dirname source_str in
  let _ = Fs.mkdir_p path in (* TO-DO handle result *)

  let* () = Lwt_io.printf "Downloading source %s\n" source_str in
  let* source = fetch source 5. in
  match source with
  | None -> Lwt_io.printf "[ERROR] Timout occured while downloading source: %s" source_str
  | Some content ->
      Lwt_io.with_file ~mode:Lwt_io.Output source_str (fun oc ->
        Lwt_io.write oc content
      )

(*<><> Util <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>*)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s [url]\n" Sys.argv.(0); 
    exit 1)
  else
  Lwt_main.run begin
    (* get the website in string form *)
    let uri = Uri.of_string Sys.argv.(1) in
    let* _, body = Cohttp_lwt_unix.Client.get uri in 
    let* body = Cohttp_lwt.Body.to_string body in

    (* parse html and extract image links *)
    let dom_node = Soup.parse body in
    let images = image_sources uri dom_node |> List.map Uri.to_string in
    let css_sources = style_sheets uri dom_node |> List.map Uri.to_string in

    Lwt.ignore_result @@ Lwt_io.printf "Inspecting:\n%s\n" Sys.argv.(1);

    Lwt.ignore_result @@ Lwt_io.printf "Images Found:\n";
    Lwt.ignore_result @@ Lwt_list.iter_p (Lwt_io.printf "%s\n") images;

    Lwt.ignore_result @@ Lwt_io.printf "Style Pages Found:\n";
    Lwt.ignore_result @@ Lwt_list.iter_p (Lwt_io.printf "%s\n") css_sources;

    let store_list = Lwt_list.iter_p (fun s -> Uri.of_string s |> store_source) in

    Lwt.join [
      store_source uri;
      store_list images;
      store_list css_sources;
    ]
  end
