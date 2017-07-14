Oracles are an crypto-economic problem. TC is a cryptographic solution that ignores the economic reality of oracles.

Some problems with TC:
1) we are trusting intel to delete the private key. If intel does not, then intel can make the TC say anything.
2) we are trusting that it is actually impossible to extract the private key from the hardware, which is doubtful. To the best of my knowledge, it is always possible to find a piece of copper where if you measure the charge during signatures you can extract the private key.
3) we are trusting the server operator not to man-in-the-middle attack his own server. For example, if SGX wants to access the price on website B, the server operator could man in the middle between his server and website B, so he could edit the data from website B to trick the server.
4) we are trusting the website operators not to lie to the SGX server.
It is possible to make a website that says "true" to most people, but says "false" to one specific customer.
