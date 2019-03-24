Using the Oracle
=========



Basic scalar oracle: `P = the price of USD in VEO from 0 to 0.05 on January 1, 2019, at 5:00 AM GMT; `

Scalar oracle with front-running protections:
`P1 = the price of USD in VEO on December 3, 2019, at 5:00 AM GMT?; P2 = the price of USD in VEO on January 1, 2019, at 5:00 AM GMT; min(1023, 512 * (P2 - P1) / P1)`