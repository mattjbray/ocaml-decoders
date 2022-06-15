open Format

let () = printf "Hello@."

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
