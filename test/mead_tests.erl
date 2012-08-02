-module(mead_tests).

-compile({parse_transform, mead}).
-compile(export_all).


map(X = F, [H|T]) ->
    [F(H)|me(_, T)];
map(F, []) ->
    [].


%type(C) when C >= $a, C =< $z -> lower;
%type(C) when C >= $0, C =< $9 -> digit;
%type(C) when C >= $A, C =< $Z -> upper.
%
%
%explode_types_cycle(T, S) ->
%    lists:reverse(explode_types_cycle(T, S, [], [])).
%
%explode_types_cycle(_Types, [], [], Res) -> Res;                
%explode_types_cycle(_Types, [], [_|_] = Buf, Res) -> [Buf|Res]; 
%explode_types_cycle(Types, [Char|Str], Buf, Res) ->             
%    case lists:member(type(Char), Types) of             
%    true  -> me(_, Str, [], [Buf|Res]);    
%    false -> me(_, Str, [Char|Buf], _)   
%    end.                                                        

%explode_types_cycle([lower], "012a345a678").
                                                                
%explode_types_cycle(_Types, [], [], Res) -> Res;                
%explode_types_cycle(_Types, [], [_|_] = Buf, Res) -> [Buf|Res]; 
%explode_types_cycle(Types, [Char|Str], Buf, Res) ->             
%    case lists:member(type(Char), Types) of             
%    true  -> explode_types_cycle(Types, Str, [], [Buf|Res]);    
%    false -> explode_types_cycle(Types, Str, [Char|Buf], Res)   
%    end.                                                        



-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

-endif.
