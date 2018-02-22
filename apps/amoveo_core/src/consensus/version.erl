-module(version).
-export([doit/1]).
%This is to make transaction replay protection easy. When we fork, we don't want the same txs to be valid on both sides. 

doit(_Height) -> 12.
