Oracle Decision Tree
===========

start:
  new oracle tx is published.
  enough blocks pass so that oracle reporting can begin
  if someone makes an honest report, goto honest_state.
  else if someone makes a dishonest report, goto dishonest_state.
  else goto default_state.

honest_state:
  if someone makes a dishonest report and locks up more money than is currently locked on the honest side of the oracle, goto dishonest_state. There can be a soft update preventing dishonest reports for specific oracles where attackers are consistently making dishonest reports.
  if enough time passes in the honest state, eventually it becomes possible to close the oracle in the honest state. goto closed_oracle.

dishonest_state:
  if someone makes an honest report and locks up more money thant is currently locked on the dishonest side, then goto honest_state.
  if enough time passes in dishonest state, it eventually becomes possible to close the oracle in the dishonest state. goto closed_oracle. There can be a soft update preventing any tx that would close the oracle in a dishonest state.

default_state:
  There are 3 ways to bet in the oracle, true/false/bad_question. for any oracle, hopefully 2 are dishonest, and 1 is honest.
  If no one has yet made any bets, it starts in the default state of bad_question. If bad_question is honest, go to honest_state, otherwise go to dishonest_state

closed_oracle:
  the final result of the oracle is recorded onto the blockchain. Everyone who made reports that agree with this result, their money doubles. If you made reports that disagree with the result, you lose the money that you had locked into the oracle.