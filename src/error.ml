type 'i t =
  | E of
      { msg : string
      ; context : 'i option
      }
  | Tag of string * 'i t
  | Group of 'i t list

let make ?context msg = E { msg; context }

let tag msg e = Tag (msg, e)

let group es = Group es

let tag_group msg es = tag msg (group es)

let rec pp ~pp_i fmt =
  let open Format in
  function
  | E { msg; context = None } ->
      fprintf fmt "@[%s@]" msg
  | E { msg; context = Some context } ->
      fprintf fmt "@[%s, but got@ @[%a@]@]" msg pp_i context
  | Tag (msg, e) ->
      fprintf fmt "@[<2>%s:@ %a@]" msg (pp ~pp_i) e
  | Group es ->
      let max_errors = 5 in
      let es_trunc = Decoders_util.My_list.take max_errors es in
      let not_shown = List.length es - max_errors in
      fprintf
        fmt
        "@[%a %s@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space (pp ~pp_i))
        es_trunc
        ( if not_shown > 0
        then Printf.sprintf "(...%d errors not shown...)" not_shown
        else "" )


let map_tag f = function Tag (s, e) -> Tag (f s, e) | e -> e
