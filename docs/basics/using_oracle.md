Using the Oracle
=========


binary oracle: `Trump won the 2020 election.`

scalar oracle: `P = the price of USD in VEO from 0 to 0.05 on January 1, 2019, at 5:00 AM GMT; return P * 1024 / 0.05`

Scalar oracle with free-option protections:
`P1 = the price of USD in VEO on December 3, 2019, at 5:00 AM GMT?; P2 = the price of USD in VEO on January 1, 2019, at 5:00 AM GMT; return min(1023, 512 * (1 + ((P2 - P1) / P1)))`