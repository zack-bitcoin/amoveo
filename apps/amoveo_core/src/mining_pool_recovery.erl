-module(mining_pool_recovery).
-export([earned_in_range/2, scan_range/3, check_height/1]).


%Ramon payment.
%has pubkeys:
%BKgyo7skE9wcEHRBLElAth3bXgYjSzoja980tkjjOk1kvq1XL2/uq/n88ys2eGHuorCtlRKnLDG0T88UrHIZ+7E=
%BHGWh9mqsQZStlZp5foxBZSQM4ociYI8eD+J+rqMkYQ/F/7WMpt2O2NsBkBFcTrNc+IE93ZkU49fwA/jFC2JPys=
%3.17840471 + 11.06353813 + 19.07663571.
%33.31857855
%pool2:
%  BKgyo7skE9wcEHRBLElAth3bXgYjSzoja980tkjjOk1kvq1XL2/uq/n88ys2eGHuorCtlRKnLDG0T88UrHIZ+7E= 3.17840471
%pool1: 
% BKgyo7skE9wcEHRBLElAth3bXgYjSzoja980tkjjOk1kvq1XL2/uq/n88ys2eGHuorCtlRKnLDG0T88UrHIZ+7E= 19.07663571
% BHGWh9mqsQZStlZp5foxBZSQM4ociYI8eD+J+rqMkYQ/F/7WMpt2O2NsBkBFcTrNc+IE93ZkU49fwA/jFC2JPys= 11.06353813


%pool2 miners to get this much veo
%  BF6npb0GNhpAXJZASh9Jncx5Fou18jP43UiB/Cjpo+Jt9hcSRm6zp2ihnm6hblcty+pBawK09SeQlPReJxK8sOw= 4.712806995
%  BFB3qHkLoxQK4IiQvwEqBo64saIw3Sk5w7JBLHldimUIwQlFv7ni4vkph7kl4X5PTPa9ra8Z4EEPJeSybBohE5Q= 48.77207239
%  BJ37iobqiuD841SprjP078J6U8PthhGL/bSC54h1DmDrj13hRZmKll1EyMP7y8EeT7caFY6TVInHaRyS1jLhYpE= 5.91840878

%pool1 miners to get this much veo
% BEB/9uVjQrLVkXtBKHCmY+/+Pn3ih8IIwfzdex0wYqRkcRbdiJSkTqfI+YiNzyjgSKCGOPR+yhLg4zlfiFoWNjo= 5.55453354
% BF6npb0GNhpAXJZASh9Jncx5Fou18jP43UiB/Cjpo+Jt9hcSRm6zp2ihnm6hblcty+pBawK09SeQlPReJxK8sOw= 7.01146038
% BFB3qHkLoxQK4IiQvwEqBo64saIw3Sk5w7JBLHldimUIwQlFv7ni4vkph7kl4X5PTPa9ra8Z4EEPJeSybBohE5Q= 9.33343752
% BJ37iobqiuD841SprjP078J6U8PthhGL/bSC54h1DmDrj13hRZmKll1EyMP7y8EeT7caFY6TVInHaRyS1jLhYpE= 8.96920581
% BMwXC8If4nGPbItlUy0+WafXWq7VPOjCSaA00cdkuO697ifzGeKq4ePyiCdU4dYp2vdtqO22Bb1l7q+trC04eT8= 8.33180032






%pool2_payments is the log from the second pool. It starts at height 268787, and ends at around 303000

%pool1_payments is much older. it starts earlier than 180000.

%pool1_slice is an approximate slice of pool1_payments that attempts to start at 288000 and end at 303000.
%pool2_slice is an approximate slice of pool2_payments that attempts to start at 288000 and end at 303000.

%cat pool2_slice_addresses | sort | uniq -c

%pool2 has these miners, with this many payments made.
%     43 BF6npb0GNhpAXJZASh9Jncx5Fou18jP43UiB/Cjpo+Jt9hcSRm6zp2ihnm6hblcty+pBawK09SeQlPReJxK8sOw= 
%    445 BFB3qHkLoxQK4IiQvwEqBo64saIw3Sk5w7JBLHldimUIwQlFv7ni4vkph7kl4X5PTPa9ra8Z4EEPJeSybBohE5Q= 
%     54 BJ37iobqiuD841SprjP078J6U8PthhGL/bSC54h1DmDrj13hRZmKll1EyMP7y8EeT7caFY6TVInHaRyS1jLhYpE= 
%     29 BKgyo7skE9wcEHRBLElAth3bXgYjSzoja980tkjjOk1kvq1XL2/uq/n88ys2eGHuorCtlRKnLDG0T88UrHIZ+7E= 
% total: 571

%pool1 miners, with this many payments made.
%    244 BEB/9uVjQrLVkXtBKHCmY+/+Pn3ih8IIwfzdex0wYqRkcRbdiJSkTqfI+YiNzyjgSKCGOPR+yhLg4zlfiFoWNjo= 
%    308 BF6npb0GNhpAXJZASh9Jncx5Fou18jP43UiB/Cjpo+Jt9hcSRm6zp2ihnm6hblcty+pBawK09SeQlPReJxK8sOw=
%    410 BFB3qHkLoxQK4IiQvwEqBo64saIw3Sk5w7JBLHldimUIwQlFv7ni4vkph7kl4X5PTPa9ra8Z4EEPJeSybBohE5Q= 
%    486 BHGWh9mqsQZStlZp5foxBZSQM4ociYI8eD+J+rqMkYQ/F/7WMpt2O2NsBkBFcTrNc+IE93ZkU49fwA/jFC2JPys= 
%    394 BJ37iobqiuD841SprjP078J6U8PthhGL/bSC54h1DmDrj13hRZmKll1EyMP7y8EeT7caFY6TVInHaRyS1jLhYpE= 
%    838 BKgyo7skE9wcEHRBLElAth3bXgYjSzoja980tkjjOk1kvq1XL2/uq/n88ys2eGHuorCtlRKnLDG0T88UrHIZ+7E= 
%    366 BMwXC8If4nGPbItlUy0+WafXWq7VPOjCSaA00cdkuO697ifzGeKq4ePyiCdU4dYp2vdtqO22Bb1l7q+trC04eT8= 
% total: rewards paid 3046

% overearned
% {69.34061143, 62.581692890}



%Pool one keeps %5 of the rewards.
%"BAgC4Jy+4mEUNnDNJ5tp3A3pZ365GXpi4GIkrbsZw0ssK9xyerHw2Aumhys4vZRY1AxNWjU+62t/yjw9VezZCfM="
-define(Pool1, 
<<4,8,2,224,156,190,226,97,20,54,112,205,39,155,105,
  220,13,233,103,126,185,25,122,98,224,98,36,173,187,
  25,195,75,44,43,220,114,122,177,240,216,11,166,135,
  43,56,189,148,88,212,12,77,90,53,62,235,107,127,202,
  60,61,85,236,217,9,243>>).


%Pool two keeps %5 of the rewards.
%"BJJxz7fHEmpuM5zo8I4EPeG6lY9DKXsf8/SwpVwW9JyiXBaTwS0gekjrNQ/lYuUGVlqlqqPFLIfD0rNUSA6g6pU="
-define(Pool2,
<<4,146,113,207,183,199,18,106,110,51,156,232,240,142,
  4,61,225,186,149,143,67,41,123,31,243,244,176,165,
  92,22,244,156,162,92,22,147,193,45,32,122,72,235,53,
  15,229,98,229,6,86,90,165,170,163,197,44,135,195,
  210,179,84,72,14,160,234,149>>).

-define(block_reward, 10382390).

-record(db, {pool1 = 0, pool2 = 0}).

check_height(N) ->
  P1 = element(2, hd(element(11, block:get_by_height(
                                   N)))),
  case P1 of
      ?Pool1 -> 1;
      ?Pool2 -> 2;
      _ -> 0
  end.

scan_range(A, A, D) -> D;
scan_range(A, E, DB = #db{pool1 = P1, pool2 = P2})
  when A < E -> 
    DB2 = case check_height(A) of
              1 -> DB#db{pool1 = P1 + 1};
              2 -> DB#db{pool2 = P2 + 1};
              0 -> DB
          end,
    scan_range(A+1, E, DB2).
            
earned_in_range(A, B) ->
    #db{pool1 = P1, pool2 = P2} = 
        scan_range(A, B, #db{}),
    %div by 20 to get 5%.
    {(P1 * ?block_reward) div 20,
     (P2 * ?block_reward) div 20}.


%mining_pool_recovery:earned_in_range(288000, 303000).
%{6001021420,1040834597}
%{6566342555,1220449944}
% in veo it is {65.66342555,12.20449944}


%at block 288000 pool1 had 150.94423138 veo
%at block 288008 pool2 had  31.28644969 veo

%at block 303000 pool1 had appx 285.94826836
%at block 303000 pool2 had appx 106.07264202

%earned in gap
% {135.00403698, 74.78619233}
% overearned
% {69.34061143, 62.581692890}



%I looked at the txs that are related to each pool, according to the explorer.

%pool2 made 2 unusual payments that don't look like they were for mining:
%big spend "G+0T7zmkAnesJQNx1q9nul8i8k1BlfPwFL+bWsdoKO0=" 5.00000000 this one is confirmed going to a miner.
%big spend "ewNPKECpOOGDkJc81R9sooqQUjOEFOupW/WbfXquVeE=" 1.67007337

%pool1 made 2 unusual spend that look like they might not be for mining:
%big spend "tfQaWAojnSZz9bCOMJKdVq/s4a7RsiOWkxhwMQhyZMQ=" 1.28781302
%big spend "f9OOzhxNKVymJbOnOnSdWVrC3lvsmiYeeM84LANeTYE=" 1.28781302


