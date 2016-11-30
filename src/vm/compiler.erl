-module(compiler).
-export([compile/1]).

-define(or_die, compile(<<" not if crash else then ">>)).
-define(plus_store, compile(<<" dup @ rot + swap ! ">>)).
compile(A) ->
    %Test to make sure : and ; are alternating the whole way, or give an intelligent error.
    %Give error message if we define the same function twice.
    B = << <<" ">>/binary, A/binary, <<" \n">>/binary>>,
    C = remove_comments(B),
    D = add_spaces(C),
    Words = to_words(D, <<>>, []),
    Macros = get_macros(Words),
    YWords = remove_macros(Words),
    ZWords = apply_macros(Macros, YWords),
    Functions = get_functions(ZWords),
    AWords = remove_functions(ZWords),
    BWords = apply_functions(AWords, Functions),
    reuse_name_check(Macros, Functions),
    to_opcodes(BWords, Functions, []).
add_spaces(B) -> add_spaces(B, <<"">>).
reuse_name_check(Macros, Functions) ->
    MacroKeys = dict:fetch_keys(Macros),
    FunctionKeys = dict:fetch_keys(Functions),
    L = repeats(MacroKeys ++ FunctionKeys),
    Bool = 0 == length(L),
    if
	Bool -> ok;
	true -> io:fwrite("error. you reused a name more than once."),
		io:fwrite(packer:pack(L)),
		Bool == true
    end.
repeats([]) -> [];
repeats([H|T]) -> 
    B = is_in(H, T),
    if
	B -> [H|repeats(T)];
	true -> repeats(T)
    end.
is_in(_, []) -> false;
is_in(A, [A|_]) -> true;
is_in(A, [_|T]) -> is_in(A, T).
add_spaces(<<"">>, B) -> B;
add_spaces(<<91:8, B/binary >>, Out) ->  % "["
    add_spaces(B, <<Out/binary, 32:8, 91:8, 32:8>>);
add_spaces(<<93:8, B/binary >>, Out) ->  % "]"
    add_spaces(B, <<Out/binary, 32:8, 93:8, 32:8>>);
add_spaces(<<58:8, B/binary >>, Out) ->  % ":"
    add_spaces(B, <<Out/binary, 32:8, 58:8, 32:8>>);
add_spaces(<<59:8, B/binary >>, Out) ->  % ";"
    add_spaces(B, <<Out/binary, 32:8, 59:8, 32:8>>);
add_spaces(<<44:8, B/binary >>, Out) ->  % ","
    add_spaces(B, <<Out/binary, 32:8, 44:8, 32:8>>);
add_spaces(<<X:8, B/binary >>, Out) -> 
    add_spaces(B, <<Out/binary, X:8>>).
remove_comments(B) -> remove_comments(B, <<"">>).
remove_comments(<<"">>, Out) -> Out;
remove_comments(<<40:8, B/binary >>, Out) -> % [40] == "(".
    C = remove_till(41, B), % [41] == ")".
    remove_comments(C, Out);
remove_comments(<<37:8, B/binary >>, Out) -> % [37] == "%".
    C = remove_till(10, B),
    remove_comments(C, Out);
remove_comments(<<X:8, B/binary>>, Out) -> 
    remove_comments(B, <<Out/binary, X:8>>).
remove_till(N, <<N:8, B/binary>>) -> B;
remove_till(N, <<_:8, B/binary>>) -> 
    remove_till(N, B).
remove_macros(Words) -> remove_macros(Words, []).
remove_macros([], Out) -> Out;
remove_macros([<<"macro">>|Words], Out) ->
    {_, B} = split(<<";">>, Words),
    remove_macros(B, Out);
remove_macros([W|Words], Out) ->
    remove_macros(Words, Out ++ [W]).
apply_macros(Macros, Words) -> apply_macros(Macros, Words, []).
apply_macros(_, [], Out) -> Out;
apply_macros(Macros, [W|Words], Out) -> 
    NOut = case dict:find(W, Macros) of
	       error -> Out ++ [W];
	       {ok, Val} -> Out ++ Val
	   end,
    apply_macros(Macros, Words, NOut).
get_macros(Words) ->
    get_macros(Words, dict:new()).
get_macros([<<"macro">>|[Name|R]], Functions) ->
    case dict:find(Name, Functions) of
	error ->
	    {Code, T} = split(<<";">>, R),
	    Code2 = apply_macros(Functions, Code),
	    NewFunctions = dict:store(Name, Code2, Functions),
	    get_macros(T, NewFunctions);
	{X, _} ->
	    io:fwrite("can't name 2 macros the same. reused name: "),
	    io:fwrite(Name),
	    io:fwrite("\n"),
	    X = okay
    end;
get_macros([], Functions) -> Functions;
get_macros([_|T], Functions) -> get_macros(T, Functions).
get_functions2(Foo, R, Priv, Name, Functions) ->
    Signature = testnet_sign:sign(Foo, base64:decode(Priv)),
    get_functions3(Signature, R, Name, Functions).
get_functions3(Signature, R, Name, Functions) ->
    case dict:find(Name, Functions) of
	error ->
	    NewFunctions = dict:store(Name, Signature, Functions),
	    get_functions(R, NewFunctions);
	{X, _} ->
	    io:fwrite("can't name 2 functions the same. reused name: "),
	    io:fwrite(Name),
	    io:fwrite("\n"),
	    X = okay
    end.
get_functions(Words) -> get_functions(Words, dict:new()).
get_functions([<<"macroSign">>|[Name|[<<"binary">>|[Priv|[<<"binary">>|[Hash|R]]]]]], Functions) -> 
    %Make sure Name isn't on the restricted list.
    Foo = base64:decode(Hash),
    get_functions2(Foo, R, Priv, Name, Functions);
get_functions([<<"macroSign">>|[Name|[<<"binary">>|[Priv|[Hash|R]]]]], Functions) ->
    %Make sure Name isn't on the restricted list.
    Foo = hd(to_opcodes([Hash], Functions, [])),
    get_functions2(Foo, R, Priv, Name, Functions);
get_functions([<<":">>|[Name|R]], Functions) ->
    %Make sure Name isn't on the restricted list.
    {Code, T} = split(<<";">>, R),
    Opcodes = to_opcodes(Code, Functions, []),
    S = hash:doit(Opcodes),
    get_functions3(S, T, Name, Functions);
get_functions([], Functions) -> Functions;
get_functions([_|T], Functions) -> get_functions(T, Functions).
split(C, B) -> split(C, B, []).
split(C, [C|B], Out) -> {flip(Out), B};
split(C, [D|B], Out) ->
    split(C, B, [D|Out]).
remove_functions(Words) -> rad(Words, []).
rad([], Out) -> flip(Out);
rad([<<":">>|[_|T]], Out) -> rad(T, [<<":">>|Out]);
rad([<<"macroSign">>|[_|[<<"binary">>|[_|[<<"binary">>|[_|T]]]]]], Out) -> rad(T, Out);
rad([<<"macroSign">>|[_|[<<"binary">>|[_|[_|T]]]]], Out) -> rad(T, Out);
rad([X|T], Out) -> rad(T, [X|Out]).
apply_functions(Words, Functions) ->    
    rnf(Words, Functions, []).
rnf([], _, Out) -> flip(Out);
rnf([H|T], Functions, Out) -> 
    case dict:find(H, Functions) of
	error -> rnf(T, Functions, [H|Out]);
	{ok, Val} -> rnf(T, Functions, [Val|Out])
    end.
b2i(X) -> list_to_integer(binary_to_list(X)).
to_opcodes([<<"pub2addr">>|R], F, Out) ->
    to_opcodes(R, F, [58|Out]);
to_opcodes([<<"dupnil==">>|R], F, Out) ->
    to_opcodes(R, F, [57|Out]);
to_opcodes([<<"id2balance">>|R], F, Out) ->
    to_opcodes(R, F, [56|Out]);
to_opcodes([<<"check">>|R], F, Out) ->
    to_opcodes(R, F, [55|Out]);
to_opcodes([<<"++">>|R], F, Out) ->
    to_opcodes(R, F, [54|Out]);
to_opcodes([<<"swap">>|R], F, Out) ->
    to_opcodes(R, F, [53|Out]);
to_opcodes([<<"[">>|R], F, Out) ->
    to_opcodes(R, F, [51|Out]);
to_opcodes([<<"nil">>|R], F, Out) ->
    to_opcodes(R, F, [51|Out]);
to_opcodes([<<"id2pub">>|R], F, Out) ->
    to_opcodes(R, F, [50|Out]);
to_opcodes([<<"car">>|R], F, Out) ->
    to_opcodes(R, F, [49|Out]);
to_opcodes([<<"cons">>|R], F, Out) ->
    to_opcodes(R, F, [48|Out]);
to_opcodes([<<",">>|R], F, Out) ->
    to_opcodes(R, F, [48|[53|Out]]);
to_opcodes([<<"]">>|R], F, Out) ->
    to_opcodes(R, F, [52|[48|[53|Out]]]);
to_opcodes([<<"reverse">>|R], F, Out) ->
    to_opcodes(R, F, [52|Out]);
to_opcodes([<<"gas">>|R], F, Out) ->
    to_opcodes(R, F, [47|Out]);
to_opcodes([<<"print">>|R], F, Out) ->
    to_opcodes(R, F, [46|Out]);
to_opcodes([<<"binary">>|[B|R]], Functions, Out) ->
    to_opcodes(R, Functions, [base64:decode(B)|Out]);
to_opcodes([<<"integer">>|[I|R]], Functions, Out) ->
    to_opcodes(R, Functions, [{integer, b2i(I)}|Out]);
to_opcodes([<<"fraction">>|[T|[B|R]]], Functions, Out) ->
    to_opcodes(R, Functions, [{f, b2i(T), b2i(B)}|Out]);
to_opcodes([<<"false">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [false|Out]);
to_opcodes([<<"true">>|R], Functions, Out) ->
    to_opcodes(R, Functions, [true|Out]);
to_opcodes([<<"@">>|R], F, Out) ->
    to_opcodes(R, F, [45|Out]);
to_opcodes([<<"!">>|R], F, Out) ->
    to_opcodes(R, F, [44|Out]);
to_opcodes([<<"rem">>|R], F, Out) ->
    to_opcodes(R, F, [43|Out]);
to_opcodes([<<"match">>|R], F, Out) ->
    to_opcodes(R, F, [42|Out]);
to_opcodes([<<"recurse">>|R], F, Out) ->
    to_opcodes(R, F, [41|Out]);
to_opcodes([<<"r>">>|R], F, Out) ->
    to_opcodes(R, F, [40|Out]);
to_opcodes([<<">r">>|R], F, Out) ->
    to_opcodes(R, F, [39|Out]);
to_opcodes([<<"r@">>|R], F, Out) ->
    to_opcodes(R, F, [39|[11|[40|Out]]]);%out is all reverse.
to_opcodes([<<"call">>|R], F, Out) ->
    to_opcodes(R, F, [38|Out]);
to_opcodes([<<";">>|R], F, Out) ->
    to_opcodes(R, F, [37|Out]);
to_opcodes([<<":">>|R], F, Out) ->
    to_opcodes(R, F, [36|Out]);
to_opcodes([<<"==">>|R], F, Out) ->
    to_opcodes(R, F, [35|Out]);
to_opcodes([<<"slash">>|R], F, Out) ->
    to_opcodes(R, F, [34|Out]);
to_opcodes([<<"stack_size">>|R], F, Out) ->
    to_opcodes(R, F, [33|Out]);
to_opcodes([<<"height">>|R], F, Out) ->
    to_opcodes(R, F, [32|Out]);
to_opcodes([<<"total_coins">>|R], F, Out) ->
    to_opcodes(R, F, [31|Out]);
to_opcodes([<<"i2f">>|R], F, Out) ->
    to_opcodes(R, F, [30|Out]);
to_opcodes([<<"f2i">>|R], F, Out) ->
    to_opcodes(R, F, [29|Out]);
to_opcodes([<<"crash">>|R], F, Out) ->
    to_opcodes(R, F, [28|Out]);
to_opcodes([<<"flip">>|R], F, Out) ->
    to_opcodes(R, F, [27|Out]);
to_opcodes([<<"stripl">>|R], F, Out) ->
    to_opcodes(R, F, [26|Out]);
to_opcodes([<<"stripr">>|R], F, Out) ->
    to_opcodes(R, F, [25|Out]);
to_opcodes([<<"append">>|R], F, Out) ->
    to_opcodes(R, F, [24|Out]);
to_opcodes([<<"not">>|R], F, Out) ->
    to_opcodes(R, F, [23|Out]);
to_opcodes([<<"xor">>|R], F, Out) ->
    to_opcodes(R, F, [22|Out]);
to_opcodes([<<"or">>|R], F, Out) ->
    to_opcodes(R, F, [21|Out]);
to_opcodes([<<"and">>|R], F, Out) ->
    to_opcodes(R, F, [20|Out]);
to_opcodes([<<"then">>|R], F, Out) ->
    to_opcodes(R, F, [19|Out]);
to_opcodes([<<"else">>|R], F, Out) ->
    to_opcodes(R, F, [18|Out]);
to_opcodes([<<"if">>|R], F, Out) ->
    to_opcodes(R, F, [17|Out]);
to_opcodes([<<"pickn">>|R], F, Out) ->
    to_opcodes(R, F, [16|Out]);
to_opcodes([<<"tuckn">>|R], F, Out) ->
    to_opcodes(R, F, [15|Out]);
to_opcodes([<<"2dup">>|R], F, Out) ->
    to_opcodes(R, F, [14|Out]);
to_opcodes([<<"rot">>|R], F, Out) ->
    to_opcodes(R, F, [13|Out]);
to_opcodes([<<"tuck">>|R], F, Out) ->
    to_opcodes(R, F, [12|Out]);
to_opcodes([<<"dup">>|R], F, Out) ->
    to_opcodes(R, F, [11|Out]);
to_opcodes([<<"drop">>|R], F, Out) ->
    to_opcodes(R, F, [10|Out]);
to_opcodes([<<"^">>|R], F, Out) ->
    to_opcodes(R, F, [9|Out]);
to_opcodes([<<"=">>|R], F, Out) ->
    to_opcodes(R, F, [8|Out]);
to_opcodes([<<"<">>|R], F, Out) ->
    to_opcodes(R, F, [7|Out]);
to_opcodes([<<">">>|R], F, Out) ->
    to_opcodes(R, F, [6|Out]);
to_opcodes([<<"/">>|R], F, Out) ->
    to_opcodes(R, F, [5|Out]);
to_opcodes([<<"*">>|R], F, Out) ->
    to_opcodes(R, F, [4|Out]);
to_opcodes([<<"-">>|R], F, Out) ->
    to_opcodes(R, F, [3|Out]);
to_opcodes([<<"+">>|R], F, Out) ->
    to_opcodes(R, F, [2|Out]);
to_opcodes([<<"verify_sig">>|R], F, Out) ->
    to_opcodes(R, F, [1|Out]);
to_opcodes([<<"hash">>|R], F, Out) ->
    to_opcodes(R, F, [0|Out]);
to_opcodes([<<"or_die">>|R], F, Out) ->
    %( bool -- )
    %if bool is true, ignore. if bool is false, then crash.
    to_opcodes(R, F, flip(?or_die) ++ Out);
to_opcodes([<<"+!">>|R], F, Out) ->
    %( 5 N -- ) in this exampe N increments by 5.
    to_opcodes(R, F, flip(?plus_store) ++ Out);
to_opcodes([], _, Out) -> flip(Out);
to_opcodes([Name|R], Functions, Out) ->
    case dict:find(Name, Functions) of
	error -> 
	    if
		is_binary(Name) ->
		    to_opcodes(R, Functions, [Name|Out]);
		true ->
		    io:fwrite("undefined word "),
		    io:fwrite(base64:encode(Name)),%looking up a hash.
		    io:fwrite("\n")
	    end;
	{ok, Val} -> 
	    to_opcodes(R, Functions, [Val|Out])
    end.
to_words(<<>>, <<>>, Out) -> flip(Out);
to_words(<<>>, N, Out) -> flip([flip_bin(N)|Out]);
to_words(<<"\t", B/binary>>, X, Out) ->
    to_words(B, X, Out);
to_words(<<" ", B/binary>>, <<"">>, Out) ->
    to_words(B, <<>>, Out);
to_words(<<"\n", B/binary>>, <<"">>, Out) ->
    to_words(B, <<>>, Out);
to_words(<<" ", B/binary>>, N, Out) ->
    to_words(B, <<>>, [flip_bin(N)|Out]);
to_words(<<"\n", B/binary>>, N, Out) ->
    to_words(B, <<>>, [flip_bin(N)|Out]);
to_words(<<C:8, B/binary>>, N, Out) ->
    to_words(B, <<C:8, N/binary>>, Out).
flip_bin(B) -> flip_bin(B, <<>>).
flip_bin(<<>>, Out) -> Out;
flip_bin(<<C:8, B/binary>>, Out) -> 
    flip_bin(B, <<C:8, Out/binary>>).
flip(X) -> flip(X, []).
flip([], Out) -> Out;
flip([H|T], Out) -> flip(T, [H|Out]).
