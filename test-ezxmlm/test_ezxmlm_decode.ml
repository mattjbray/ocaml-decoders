open CCFormat

(* Example 1*)
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
    "@[<v 2>Decode a tree:@ %a@]@.@."
    (pp_print_result ~ok:pp_root ~error:pp_error)
    (decode_string root xml_str)


(* Example 2*)
let () =
  let xml_str = {|<root><!-- a comment --> Some data </root>|} in
  let decoder = tag "root" >>= fun () -> children data in
  printf
    "@[<v 2>Comments are skipped:@ %a@]@.@."
    (pp_print_result ~ok:(list string_quoted) ~error:pp_error)
    (decode_string decoder xml_str)


(* Example 3 : using pick_children to select data in a grandchild of root. *)
let () =
  let xml_str =
    {|<root><!-- a comment --> <node> Some data in nested node </node> </root>|}
  in
  let decoder : string list list decoder =
    (* To begin, the decoder is "focussed" on the top level element, in this case <root>. *)
    (* [tag "root"] asserts that the "focussed" element is the tag "root"; it will fail otherwise. *)
    tag "root"
    >>= fun () ->
    (* Now we want to descend into the children of <root>. *)
    (* [pick_children d] selects all the children for which the decoder [d] succeeds.
       If [d] fails, the child will be skipped.
       If [d] succeeds, it should return *another* decoder which will be
       used to decode the child.
    *)
    pick_children
      ( (* [tag "node"] asserts that the current element is the tag <node>. This
           will fail for other children of <root> (e.g. the whitespace data), so
           everything else will be skipped. *)
        tag "node"
      >>= fun () ->
      (* Now we are "focussed" on the <data> children, we want do return all
         the text data children of those nodes.

         We use [children] here and not [pick_children], so anything that
         fails to decode as [data] will fail the whole decoder.
      *)
      pure (children data) )
  in
  printf
    "@[<v 2>Comments are skipped:@ %a@]@.@."
    (pp_print_result ~ok:(list (list string_quoted)) ~error:pp_error)
    (decode_string decoder xml_str)


(* Example 3 : using pick_children to select data in a grandchild of root. *)
let () =
  let xml_str =
    {|<root><!-- a comment --> <node1> Some data </node1> <node2> More data </node2> </root>|}
  in
  let decoder : string list list decoder =
    (* To begin, the decoder is "focussed" on the top level element, in this case <root>. *)
    (* [tag "root"] asserts that the "focussed" element is the tag "root"; it will fail otherwise. *)
    tag "root"
    >>= fun () ->
    (* Now we want to descend into the children of <root>. *)
    (* [pick_children d] selects all the children for which the decoder [d] succeeds.
       If [d] fails, the child will be skipped.
       If [d] succeeds, it should return *another* decoder which will be
       used to decode the child.
    *)
    pick_children
      ( (* [tag "node"] asserts that the current element is the tag <node>. This
           will fail for other children of <root> (e.g. the whitespace data), so
           everything else will be skipped. *)
        tag "node"
      >>= fun () ->
      (* Now we are "focussed" on the <data> children, we want do return all
         the text data children of those nodes.

         We use [children] here and not [pick_children], so anything that
         fails to decode as [data] will fail the whole decoder.
      *)
      pure (children data) )
  in
  printf
    "@[<v 2>Comments are skipped:@ %a@]@.@."
    (pp_print_result ~ok:(list (list string_quoted)) ~error:pp_error)
    (decode_string decoder xml_str)


(* Example 4 *)

let root_decoder =
  let open Decoders_ezxmlm.Decode in
  let node2_decoder =
    pick_children (any_tag >>= fun _ -> succeed @@ children data)
    >|= List.concat
  in
  tag "root"
  >>= fun () ->
  pick_children
    ( any_tag
    >>= fun nm ->
    match nm with
    | "node2" ->
        tag "node2" >>= fun () -> succeed @@ node2_decoder
    | _ ->
        fail "invalid node" )
  >|= List.concat


let () =
  let open Decoders_ezxmlm.Decode in
  let xml_str =
    {|<root><!-- a comment --> <node1> Some data </node1> <node2> <boo>More data</boo> <bla> even more data </bla> </node2> </root>|}
  in
  let ret = decode_string root_decoder xml_str in
  match ret with
  | Ok fld ->
      printf "%a" CCFormat.(list ~sep:(return ",") string) fld
      (* this prints "More data, even more data" *)
  | Error e ->
      failwith @@ string_of_error e
