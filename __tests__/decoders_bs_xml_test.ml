open Jest
open Xml_dom

let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "tag"
          Decode.(
            fun () ->
              let xml_str = {|<root></root>|} in
              let decoded = decode_string (tag "root") xml_str in
              expect decoded |> toEqual (Belt.Result.Ok ())))


let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "empty attrs"
          Decode.(
            fun () ->
              let xml_str = {|<root></root>|} in
              let decoded = decode_string attrs xml_str in
              expect decoded |> toEqual (Belt.Result.Ok [])))


let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "non-empty attrs"
          Decode.(
            fun () ->
              let xml_str = {|<root id="1"></root>|} in
              let decoded = decode_string attrs xml_str in
              expect decoded |> toEqual (Belt.Result.Ok [ ("id", "1") ])))


let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "attr_opt none"
          Decode.(
            fun () ->
              let xml_str = {|<root></root>|} in
              let decoded = decode_string (attr_opt "id") xml_str in
              expect decoded |> toEqual (Belt.Result.Ok None)))


let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "attr_opt some"
          Decode.(
            fun () ->
              let xml_str = {|<root id="1"></root>|} in
              let decoded = decode_string (attr_opt "id") xml_str in
              expect decoded |> toEqual (Belt.Result.Ok (Some "1"))))


let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "attr fail"
          Decode.(
            fun () ->
              let xml_str = {|<root></root>|} in
              let decoded = decode_string (attr "id") xml_str in
              expect (match decoded with Ok _ -> false | Error _ -> true)
              |> toBe true))


let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "attr succeed"
          Decode.(
            fun () ->
              let xml_str = {|<root id="1"></root>|} in
              let decoded = decode_string (attr "id") xml_str in
              expect decoded |> toEqual (Belt.Result.Ok "1")))


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
  ; trees : (string * node) array
  }

and node =
  | Action of
      { id : string
      ; attrs : (string * string) array
      }
  | Sequence of node array
  | Subtree of { id : string }

let xml_tree =
  { main = "MainTree"
  ; trees =
      [| ( "MainTree"
         , Sequence
             [| Action
                  { id = "SayA"
                  ; attrs = [| ("message", "Hello World"); ("env", "cruel") |]
                  }
              ; Subtree { id = "GraspObject" }
             |] )
       ; ( "GraspObject"
         , Sequence
             [| Action { id = "Open"; attrs = [||] }
              ; Action { id = "Approach"; attrs = [||] }
              ; Action { id = "Close"; attrs = [||] }
              ; Subtree { id = "DestroyObject" }
             |] )
       ; ("DestroyObject", Sequence [| Action { id = "Booom"; attrs = [||] } |])
      |]
  }


open Decode

let rec node node_ty : node decoder =
  match node_ty with
  | "Action" ->
      attr "ID"
      >>= fun id ->
      attrs
      >>= fun attrs ->
      let attrs =
        attrs |> List.filter (fun (name, _) -> name <> "ID") |> Array.of_list
      in
      succeed (Action { id; attrs })
  | "Sequence" ->
      pick_children (any_tag >>= fun node_ty -> pure (node node_ty))
      >>= fun nodes -> succeed (Sequence (Array.of_list nodes))
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
  >>= fun trees -> succeed { main; trees = Array.of_list trees }


let () =
  describe
    "decoders-bs-xml decode"
    Expect.(
      fun () ->
        test
          "tree"
          Decode.(
            fun () ->
              let decoded = decode_string root xml_str in
              expect decoded |> toEqual (Belt.Result.Ok xml_tree)))


open Encode

let rec node = function
  | Action { id; attrs } ->
      tag "Action" ~attrs:(("ID", id) :: Array.to_list attrs) []
  | Sequence nodes ->
      tag "Sequence" (Array.to_list (Array.map node nodes))
  | Subtree { id } ->
      tag "Subtree" ~attrs:[ ("ID", id) ] []


let tree (name, n) = tag "BehaviorTree" ~attrs:[ ("ID", name) ] [ node n ]

let root t =
  tag
    "root"
    ~attrs:[ ("main_tree_to_execute", t.main) ]
    (Array.to_list (Array.map tree t.trees))


let () =
  describe
    "decoders-bs-xml encode"
    Expect.(
      fun () ->
        test "tree" (fun () ->
            let encoded = encode_string root xml_tree in
            let expected =
              Js.String.splitByRe [%re "/\\n/"] xml_str
              |> Array.to_list
              |> Decoders.Util.My_list.filter_map (function
                     | None ->
                         None
                     | Some line ->
                         Some (Js.String.trim line) )
              |> String.concat ""
            in

            expect encoded |> toEqual expected ))
