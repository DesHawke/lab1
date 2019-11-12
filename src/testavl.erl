%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. нояб. 2019 9:54
%%%-------------------------------------------------------------------
-module(testavl).
-author("user").
-compile(export_all).
%%-export(export_all).


%%%*_ MACROS and SPECS =========================================================

-define(undef, undefined).
-define(empty_tree, #node{key = ?undef, left = ?undef, right = ?undef, height = 0}).

-record(node,
{ key,
  left,
  right,
  height
}).

-type tree() :: #node{} | ?undef.

%%%*_ API FUNCTIONS ============================================================

-spec new() -> tree().
new() -> ?empty_tree.

-spec new([term()]) -> tree().
new(KVList) -> lists:foldl(
  fun(Key, Acc) -> insert(Acc, Key) end,
    new(), KVList).

-spec insert(tree(), term()) -> tree().
insert(OldTree, Key) when OldTree == ?empty_tree; OldTree == ?undef -> #node{key = Key, height = 1};

insert(#node{ key = Root, left = Left } = OldTree, Key) when Key < Root ->
      update_tree(OldTree#node{left = insert(Left, Key)}, Key, insert_left);
insert(#node{ key = Root, right = Right } = OldTree, Key) when Key > Root ->
      update_tree(OldTree#node{right = insert(Right, Key)}, Key, insert_right);
insert(_, _) -> erlang:error(badarg).

-spec delete(tree(), term()) -> tree().
delete(T, _) when T == ?empty_tree; T == ?undef -> T;
delete(#node{ key = Root, left = Left, right = Right} = OldTree, Key) ->
  if
    Key == Root ->
      case Right of
        ?undef -> Left;
        _      ->
          {NewRoot} = get_min(Right),
          update_tree(OldTree#node{ key = NewRoot, right = deletemin(Right)}, delete_right)
      end;
    Key > Root ->
      update_tree(OldTree#node{right = delete(Right, Key)},
        delete_right);
    Key < Root ->
      update_tree(OldTree#node{left = delete(Left, Key)},
        delete_left)
  end.

-spec prev_traverse(tree()) -> [term()].
prev_traverse(?undef) -> [];
prev_traverse(#node{key = Root, left = ?undef, right = ?undef }) -> [ Root ];
prev_traverse(#node{key = Root, left = ?undef, right = Right  }) -> [ Root | prev_traverse(Right) ];
prev_traverse(#node{key = Root, left = Left,   right = ?undef }) -> [ Root | prev_traverse(Left) ];
prev_traverse(#node{key = Root, left = Left,   right = Right  }) -> [ Root ] ++ prev_traverse(Left) ++ prev_traverse(Right).

-spec mid_traverse(tree()) -> [term()].
mid_traverse(?undef) -> [];
mid_traverse(#node{key = Root, left = ?undef, right = ?undef }) -> [ Root ];
mid_traverse(#node{key = Root, left = ?undef, right = Right  }) -> [ Root | mid_traverse(Right)];
mid_traverse(#node{key = Root, left = Left,   right = ?undef }) -> mid_traverse(Left) ++ [ Root ];
mid_traverse(#node{key = Root, left = Left,   right = Right  }) -> mid_traverse(Left) ++ [ Root ] ++ mid_traverse(Right).

-spec last_traverse(tree()) -> [term()].
last_traverse(?undef) -> [];
last_traverse(#node{key = Root, left = ?undef, right = ?undef }) -> [ Root ];
last_traverse(#node{key = Root, left = ?undef, right = Right  }) -> last_traverse(Right) ++ [ Root ];
last_traverse(#node{key = Root, left = Left,   right = ?undef }) -> last_traverse(Left) ++ [ Root ];
last_traverse(#node{key = Root, left = Left,   right = Right  }) -> last_traverse(Left) ++ last_traverse(Right) ++ [ Root ].


%%%*_ PRIVATE FUNCTIONS ========================================================

update_tree(#node{ left = Left, right = Right } = Tree, Key, insert_left) ->
  case height(Left) - height(Right) == 2 of
    true ->
      if
        Key < Left#node.key ->
          left_left_rotation(Tree);
        true ->
          left_right_rotation(Tree)
      end;
    false ->
      update_height(Tree)
  end;
update_tree(#node{ left = Left, right = Right } = Tree, Key, insert_right) ->
  case height(Right) - height(Left) == 2 of
    true ->
      if Key > Right#node.key -> right_right_rotation(Tree);
        true -> right_left_rotation(Tree)
      end;
    false ->
      update_height(Tree)
  end.

get_min(#node{ key = Root, left = ?undef}) ->  Root;
get_min(#node{left = Left}) -> get_min(Left).

deletemin(?empty_tree = T) -> T;
deletemin(#node{left = ?undef, right = Right}) -> Right;
deletemin(#node{left = Left} = OldTree) ->
  case Left#node.left of
    ?undef ->
      %% found min
      update_tree(OldTree#node{left = Left#node.right},
        delete_left);
    _ ->
      update_tree(OldTree#node{left = deletemin(Left)},
        delete_left)
  end.

update_tree(#node{ left = Left
  , right = Right
} = Tree,
    delete_left) ->
  case height(Right) - height(Left) == 2 of
    true ->
      case height(Right#node.left) > height(Right#node.right) of
        true ->
          right_left_rotation(Tree);
        false ->
          right_right_rotation(Tree)
      end;
    false ->
      update_height(Tree)
  end;
update_tree(#node{ left = Left
  , right = Right
} = Tree,
    delete_right) ->
  case height(Left) - height(Right) == 2 of
    true ->
      case height(Left#node.right) > height(Left#node.left) of
        true ->
          left_right_rotation(Tree);
        false ->
          left_left_rotation(Tree)
      end;
    false ->
      update_height(Tree)
  end.

%%
%%     4             4
%%    / \  insert   / \     LL
%%   2   5 =====>  2   5  ======>  2
%%  / \           / \             / \
%% 1   3         1   3           1   4
%%              /               /   / \
%%             0               0   3   5
%%
left_left_rotation(#node{left = OldLeft} = OldTree) ->
  NewRight = update_height(OldTree#node{left = OldLeft#node.right}),
  update_height(OldLeft#node{right = NewRight}).

%%
%%     1              1
%%    / \   insert   / \     RR
%%   0   3  =====>  0   3  ======>   3
%%      / \            / \          / \
%%     2  4           2   4        1   4
%%                         \      / \   \
%%                          5    0   2   5
%%
right_right_rotation(#node{right = OldRight} = OldTree) ->
  NewLeft = update_height(OldTree#node{right = OldRight#node.left}),
  update_height(OldRight#node{left = NewLeft}).

%%
%%     2              2               2
%%    / \  insert    / \     LL      / \    RR
%%   0   10 =====>  0  10  ======>  0   3  ====>    3
%%       / \           / \               \         / \
%%      3  11         3  11              10       2  10
%%                     \                 / \     /   / \
%%                      9               9   11  0   9  11
%%
right_left_rotation(#node{right = OldRight} = OldTree) ->
  NewRight = left_left_rotation(OldRight),
  right_right_rotation(OldTree#node{right = NewRight}).

%%
%%     8             8               8            7
%%    / \  insert   / \     RR      / \    LL    / \
%%   5   9 =====>  5   9  ======>  7   9  ====> 5   8
%%  / \           / \             /            / \   \
%% 3   7         3   7           5            3   6   9
%%                  /           / \
%%                 6           3   6
%%
left_right_rotation(#node{left = OldLeft} = OldTree) ->
  NewLeft = right_right_rotation(OldLeft),
  left_left_rotation(OldTree#node{left = NewLeft}).

update_height(#node{left = Left, right = Right} = Tree) ->
  Height = erlang:max(height(Left), height(Right)) + 1,
  Tree#node{height = Height}.

height(?undef) -> 0;
height(#node{height = Height}) -> Height.