%%%-------------------------------------------------------------------
%%% @author Egor
%%% @copyright (C) 2019, AltSTU
%%% @doc
%%%
%%% @end
%%% Created : 14. нояб. 2019 11:46
%%%-------------------------------------------------------------------
-module(modtests).
-author("Egor").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

avl_test() ->
    [
    {"prev/mid/last",
    fun() ->
      A = [3, 2, 1, 4, 5, 6, 7, 16, 15, 14, 13, 12, 11, 10, 8, 9],
      Tree = lab_avl:new(A),
      ?assertEqual([7,4,2,1,3,6,5,13,11,9,8,10,12,15,14,16], lab_avl:prev_traverse(Tree)),
      ?assertEqual([1,3,2,5,6,4,8,10,9,12,11,14,16,15,13,7], lab_avl:last_traverse(Tree)),
      ?assertEqual([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], lab_avl:mid_traverse(Tree))
    end
    },
    {"insert",
      fun() ->
        A = [3, 2, 1, 4, 5, 6, 7, 16, 15, 14, 13, 12, 11, 10, 8, 9],
        Tree = lab_avl:new(A),
        ?assertEqual([7,4,2,1,3,6,5,13,11,9,8,10,12,15,14,16], lab_avl:prev_traverse(Tree)),
        TreeIns1 = lab_avl:insert(Tree, 100),
        ?assertEqual([7,4,2,1,3,6,5,13,11,9,8,10,12,15,14,16,100], lab_avl:prev_traverse(TreeIns1)),
        TreeIns2 = lab_avl:insert(TreeIns1, 101),
        ?assertEqual([7,4,2,1,3,6,5,13,11,9,8,10,12,15,14,100,16,101], lab_avl:prev_traverse(TreeIns2)),
        TreeIns3 = lab_avl:insert(TreeIns2, 102),
        ?assertEqual([7,4,2,1,3,6,5,13,11,9,8,10,12,100,15,14,16,101, 102], lab_avl:prev_traverse(TreeIns3))
      end
    }
].

-endif.