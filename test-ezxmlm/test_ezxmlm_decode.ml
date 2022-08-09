open CCFormat

let xml_str =
  {|
<root main_tree_to_execute="MainTree">
  <BehaviorTree ID="MainTree">
    <Sequence>
      <Action ID="SayA" message="Hello World" env="cruel"/>
      <Subtree ID="GraspObject"/>
    </Sequence>
  </BehaviorTree>
  <BehaviorTree ID="GraspObject">
    <Sequence>
      <Action ID="Open"/>
      <Action ID="Approach"/>
      <Action ID="Close"/>
      <Subtree ID="DestroyObject"/>
    </Sequence>
  </BehaviorTree>
  <BehaviorTree ID="DestroyObject">
    <Sequence>
      <Action ID="Booom"/>
    </Sequence>
  </BehaviorTree>
</root>
|}


type root =
  { main : string
  ; trees : (string * node) list
  }

and node =
  | Action of
      { id : string
      ; attrs : (string * string) list
      }
  | Sequence of node list
  | Subtree of { id : string }

let rec pp_root fmt root =
  fprintf
    fmt
    "@[<1>(:main %S@ :trees @[<1>(%a)@])@]"
    root.main
    (list ~sep:(return "@ ") pp_tree)
    root.trees


and pp_tree fmt (name, node) = fprintf fmt "@[<1>(:%s %a)@]" name pp_node node

and pp_node fmt = function
  | Action { id; attrs = _ } ->
      fprintf fmt "@[<1>(:action %S)@]" id
  | Sequence nodes ->
      fprintf
        fmt
        "@[<1>(:sequence@ @[<1>(%a)@])@]"
        (list ~sep:(return "@ ") pp_node)
        nodes
  | Subtree { id } ->
      fprintf fmt "@[<1>(:subtree %S)@]" id


open Decoders_ezxmlm.Decode

let rec node node_ty : node decoder =
  match node_ty with
  | "Action" ->
      attr "ID" >>= fun id -> succeed (Action { id; attrs = [] })
  | "Sequence" ->
      pick_children (any_tag >>= fun node_ty -> pure (node node_ty))
      >>= fun nodes -> succeed (Sequence nodes)
  | "Subtree" ->
      attr "ID" >>= fun id -> succeed (Subtree { id })
  | _ ->
      fail "Unknown node type"


let tree : (string * node) decoder =
  attr "ID"
  >>= fun id ->
  pick_children (any_tag >>= fun node_ty -> pure (node node_ty))
  >>= function
  | [ node ] -> succeed (id, node) | _ -> fail "Expected a single child"


let root : root decoder =
  tag "root"
  >>= fun () ->
  attr "main_tree_to_execute"
  >>= fun main ->
  pick_children (tag "BehaviorTree" >>= fun () -> pure tree)
  >>= fun trees -> succeed { main; trees }


let pp_print_result ~ok ~error fmt = function
  | Ok x ->
      ok fmt x
  | Error e ->
      error fmt e


let () =
  printf
    "%a@."
    (pp_print_result ~ok:pp_root ~error:pp_error)
    (decode_string root xml_str)
