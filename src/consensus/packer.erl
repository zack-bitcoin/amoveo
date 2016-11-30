%We should add some rules about which atoms can be used with tuples. If peers can trick us into decoding new atoms, they can overflow erlang with too many atoms.
-module(packer).
-export([pack/1,unpack/1,test/0, untup/1, unpack_helper/1]).
-define(KEY, -6).
untup(X) when is_tuple(X) -> lists:map(fun(Z) ->untup(Z) end, tuple_to_list(X));
untup(X) when is_list(X) -> [?KEY|lists:map(fun(Z)->untup(Z) end,X)];
untup(X) when is_binary(X) -> base64:encode(X);
%untup(X) when is_binary(X) -> X; %bad
untup(X) -> X.
unpack(I) when is_integer(I) -> I;
unpack(JSON) -> unpack_helper(jiffy:decode(JSON)).
unpack_helper(J) when is_binary(J) -> base64:decode(J);
%unpack_helper(J) when is_binary(J) -> J;%bad
unpack_helper(J) when not is_list(J) -> J;
unpack_helper(J) when hd(J) == ?KEY -> 
    lists:map(fun(X) -> unpack_helper(X) end, tl(J));
unpack_helper(J) -> 
    K = hd(J),
    Out = if
	      is_binary(K) -> binary_to_atom(K, latin1);
	      is_integer(K) -> K	    
	  end,
    list_to_tuple([Out|lists:map(fun(X) -> unpack_helper(X) end, tl(J))]).
pack(X) -> jiffy:encode(untup(X)).
-record(d, {a = "", b = "" }).
test() -> 
    Record = #d{a=[1, 2, <<"abc">>, [], #d{}], b = <<1,2,3,200>> },
    %ABC = {unlock, 24001,1,[{signed,{channel_block,0,3,-9500,3,[],24001,false,259,0,0,0],"TUVZQ0lRQzlwVkxjQ0hReXhpWE0zOU43bVFOS1pTV01WS0MxMkNUYjUwZSs4MkRnd3dJaEFPZG1lWlp0VXdjUXU0UjQzazhRWkREd29tb1BuQ05TWlhDSEl0QU5PemRj",[-6],[-6]],0]],
    %New = ["unlock2",24001,1,[["signed",["channel_block",0,3,-9500,3,[-6],24001,false,259,0,0,0],"TUVZQ0lRQzlwVkxjQ0hReXhpWE0zOU43bVFOS1pTV01WS0MxMkNUYjUwZSs4MkRnd3dJaEFPZG1lWlp0VXdjUXU0UjQzazhRWkREd29tb1BuQ05TWlhDSEl0QU5PemRj",[-6],[-6]],0]],
    List = [[],3,[4]],
    Int = 123,
    Int = unpack(pack(Int)),
    List = unpack(pack(List)),
    true = is_record(unpack(pack(Record)), d),
    Record = unpack(pack(Record)),
    true = is_binary(pack(Record)),
    success.
