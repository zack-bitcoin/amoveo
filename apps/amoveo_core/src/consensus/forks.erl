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
%get(4) -> common(26900, max(test_height(), constants:retarget_frequency()));
get(4) -> common(26900, test_height());
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
get(28) -> common(99999999, 99999999);%unused.
get(29) -> common(104600, test_height());
get(30) -> common(108600, test_height());
get(31) -> common(109900, test_height());
get(32) -> common(130300, %subcurrencies
                  test_height()+1);
                  %forks:get(10) + 1);
get(33) -> common(126000, test_height());%enforcing max block size.
get(34) -> 0;%filler
get(35) -> common(130700, %automatic market makers.
                  %forks:get(32) + 1);
                  test_height()+1);
%get(36) -> common(132850, test_height());
get(36) -> common(132850, 1);%hard update to prevent market_liqudity tx from being re-published.
get(37) -> common(132860, test_height());%hard update to fix how market_liquidity_tx work with flash loans.
get(38) -> common(0, test_height());%soft update to prevent market_liquidity_tx being re-published.
get(39) -> common(133240, test_height());%the database we use while processing blocks, it needs to distinguish between a proof of non-existence, and the non-existence of a proof.
get(40) -> common(133250, test_height());%zero veo fee payments using a flash loan.
get(41) -> common(133260, test_height());%market_swap_tx dust fix.
get(42) -> common(133400, test_height());%soft fork to prevent markets from having zero liquidity.
get(43) -> common(136300, test_height());%undo #41
get(44) -> common(149729, %forks:get(35)+1);%swap_tx2, turn off channel tx types
                  test_height()+1);
get(45) -> common(152000, test_height());%trade cancel tx, ability to cancel trades that have not yet been partially matched
get(46) -> common(154000, test_height());%fix an oracle bug that sometimes caused the oracle to return 'bad question' incorrectly.
get(47) -> common(154100, test_height());%contract evidence tx and oracle new tx should charge fees based on the number of bytes.
get(48) -> common(154400, %swap receipts hard update
                  test_height()+1);
get(49) -> common(154500, %refund for money trapped in channels
                  test_height()+1);
%get(50) -> common(99999149729, %perpetual stablecoin hard update.
%                  test_height()+1);
get(_) -> 99999999999999.

top() -> top(1).
top(N) ->
    case forks:get(N) of
        none -> N-1;
        _ -> top(N+1)
    end.
            
