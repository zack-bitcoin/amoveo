Oracle ID
=========

An Oracle ID or OID is a short binary value used to identify an Oracle.
Both binary and scalar oracles have OIDs.
If you want to make a contract that references an oracle, you need to write that oracle's OID into the contract.

We often encode OIDs into base64 when we want to copy/paste them.
For example abQmqMdgpas0zRpAji1DShVFD7kauQrCrZJJC6005c4=

The OID is uniquely defined by the question being asked of the oracle, and the height at which the resolution process will begin. This way we are able to use an OID before an oracle has been created.

