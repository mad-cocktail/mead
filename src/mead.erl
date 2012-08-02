-module(mead).
-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    X = [handle_lvl1_func_def(Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


handle_lvl1_func_def(Node) ->
    case erl_syntax:type(Node) of
        function ->
            Name    = erl_syntax:function_name(Node),
            Clauses = erl_syntax:function_clauses(Node),
            NewClauses = handle_clauses(Name, Clauses),
            NewFunTree = erl_syntax:function(Name, NewClauses),
            erl_syntax:revert(NewFunTree);
        _Type -> Node
    end.


handle_clauses(FunName, Clauses) ->
    [handle_clause(FunName, Clause) || Clause <- Clauses].


handle_clause(FunName, Clause) ->
    Patterns = erl_syntax:clause_patterns(Clause),
    Arity = length(Patterns),
    NewPatterns = add_argument_variables(Patterns),
    io:format(user, "~p~n", [Patterns]),
    Guard    = erl_syntax:clause_guard(Clause),
    Body     = erl_syntax:clause_body(Clause),
    Replacer = my_function_handler(FunName, Arity),
    NewBody  = handle_group(Replacer, Body),
    erl_syntax:clause(NewPatterns, Guard, NewBody).


%% @doc Rewrite functions arguments.
%% Before:
%% map(F, [H|T]).
%% After:
%% map(_1 = F, _2 = [H|T]).
add_argument_variables(Args) ->
    add_argument_variables(Args, 1, []).


add_argument_variables([Arg|Args], Pos, Acc) ->
    NewArg = erl_syntax:match_expr(variable(Pos), Arg),
    add_argument_variables(Args, Pos+1, [NewArg|Acc]);

add_argument_variables([], _Pos, Acc) ->
    lists:reverse(Acc).


my_function_handler(Name, Arity) ->
    fun(Node) ->
        case is_local_function(Node, me, Arity) of
            false -> Node;
            true ->
                Args = erl_syntax:application_arguments(Node),
                NewArgs = rewrite_arguments_underscope(Args),
                erl_syntax:application(none, Name, NewArgs)
        end
    end.


rewrite_arguments_underscope(Args) ->
    rewrite_arguments_underscope(Args, 1, []).


rewrite_arguments_underscope([Arg|Args], Pos, Acc) ->
    NewArg = 
        case erl_syntax:type(Arg) of
            underscore -> variable(Pos); 
            _Type -> Arg 
        end,
    rewrite_arguments_underscope(Args, Pos+1, [NewArg|Acc]);

rewrite_arguments_underscope([], _Pos, Acc) ->
    lists:reverse(Acc).


variable(X) ->
    erl_syntax:variable("_" ++ integer_to_list(X)).


postorder(F, Form) ->
    NewTree =
        case erl_syntax:subtrees(Form) of
        [] ->
            Form;
        List ->
            Groups = [handle_group(F, Group) || Group <- List],
            Tree2 = erl_syntax:update_tree(Form, Groups),
            Form2 = erl_syntax:revert(Tree2),
            Form2
        end,
    F(NewTree).

                                                                    
                                                                    
-spec is_local_function(Node, FunName, FunArity) -> boolean() when  
    Node :: erl_syntax:syntaxTree(),                                
    FunName :: atom(),                                              
    FunArity :: non_neg_integer().                                  
                                                                    
is_local_function(Node, FunName, FunArity) ->                       
    erl_syntax:type(Node) =:= application                           
        andalso always(Op = erl_syntax:application_operator(Node))  
        andalso erl_syntax:type(Op) =:= atom                        
        andalso erl_syntax:atom_value(Op) =:= FunName               
        andalso application_arity(Node) =:= FunArity.               


always(_) -> true.


application_arity(AppNode) ->
    length(erl_syntax:application_arguments(AppNode)).
        
    
handle_group(F, Group) ->
    [postorder(F, Subtree) || Subtree <- Group].
