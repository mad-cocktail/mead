-module(mead_tests).

-compile({parse_transform, mead}).
-compile(export_all).


map(F, [H|T]) ->
    [F(H)|me(_, T)];
map(F, []) ->
    [].


type(C) when C >= $a, C =< $z -> lower;
type(C) when C >= $0, C =< $9 -> digit;
type(C) when C >= $A, C =< $Z -> upper.


%% From `ux_string.erl'.
explode_types_cycle_after(T, S) ->
    lists:reverse(explode_types_cycle_after(T, S, [], [])).

explode_types_cycle_before(T, S) ->
    lists:reverse(explode_types_cycle_before(T, S, [], [])).


explode_types_cycle_after(_Types, [], [], Res) -> Res;
explode_types_cycle_after(_Types, [], [_|_] = Buf, Res) -> [Buf|Res];
explode_types_cycle_after(Types, [Char|Str], Buf, Res) ->
    case lists:member(type(Char), Types) of
    true  -> me(_, Str, [], [Buf|Res]);
    false -> me(_, Str, [Char|Buf], _)
    end.


explode_types_cycle_before(_Types, [], [], Res) -> Res;
explode_types_cycle_before(_Types, [], [_|_] = Buf, Res) -> [Buf|Res];
explode_types_cycle_before(Types, [Char|Str], Buf, Res) ->
    case lists:member(type(Char), Types) of
    true  -> explode_types_cycle_before(Types, Str, [], [Buf|Res]);
    false -> explode_types_cycle_before(Types, Str, [Char|Buf], Res)
    end.


functions() ->
    [my_arguments(), my_name(), my_arity()].

functions(_) ->
    [my_arguments(), my_name(), my_arity()].

functions(_,_) ->
    [my_arguments(), my_name(), my_arity()].

fun_hof() ->
    fun_me().

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

double(X) -> X*2.

map_test_() ->
    [?_assertEqual(map(fun double/1, [1,2,3])
            ,lists:map(fun double/1, [1,2,3]))].

explode_types_cycle_test_() ->
    [?_assertEqual(explode_types_cycle_before([lower], "012a345a678")
                  ,explode_types_cycle_after([lower], "012a345a678"))].

functions_test_() ->
    [?_assertEqual(functions()
                   ,[[], functions, 0])
    ,?_assertEqual(functions(test)
                   ,[[test], functions, 1])
    ,?_assertEqual(functions(gin, rum)
                   ,[[gin, rum], functions, 2])
    ].

fun_hof_test() ->
    F = fun_hof(),
    %% Is it a function...
    ?assert(is_function(F)),
    %% ... that returns yourself?
    ?assertEqual(F, F()),
    ok.

-endif.
