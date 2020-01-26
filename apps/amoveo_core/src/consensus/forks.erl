-module(forks).
-export([get/1,top/0]).

common(A, B) ->
    case application:get_env(amoveo_core, kind) of
	{ok, "production"} -> A;
	_ -> B
    end.
test_height() -> 0.
    %50.

get(1) -> common(4200, test_height());
get(2) -> common(9000, test_height());
get(3) -> common(9900, test_height());
get(4) -> common(26900, max(test_height(), constants:retarget_frequency()));
get(5) -> common(27500, max(test_height(), 1));
get(6) -> common(27700, test_height());
get(7) -> common(28135, 40);%test_height()).
get(8) -> common(36120, test_height());
get(9) -> common(39500, test_height());
get(10) -> common(47043, 1);
get(11) -> common(58650, test_height());
get(12) -> common(39000, test_height());
get(13) -> common(61300, test_height());
get(14) -> common(62400, max(test_height(), 1));
get(15) -> common(63300, test_height());
get(16) -> common(63301, test_height());
get(17) -> common(66775, test_height());
get(18) -> common(68345, test_height());
get(19) -> common(67525, test_height());
get(20) -> common(68696, test_height());
get(21) -> common(72700, test_height());
get(22) -> common(73300, test_height());
get(23) -> common(76200, test_height());
get(24) -> common(82270, test_height());%require that new oracle ids conform to the standard.
get(25) -> common(77500, test_height());%so that we can prove the non-existence of oracles and channels and accounts.
get(26) -> common(96560, test_height());%so we can cancel channel offers.
get(27) -> common(102260, test_height());%so oracle_new tx includes all the proofs that it needs.
get(_) -> none.

top() -> top(1).
top(N) ->
    case forks:get(N) of
        none -> N-1;
        _ -> top(N+1)
    end.
            
