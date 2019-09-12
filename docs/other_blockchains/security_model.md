Blockchain Security Models
==========

The security model of a blockchain is a description of how the blockchain works. It is made at a particular level of abstraction so that the level of trust/security of the mechanism is easy to calculate. https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md


Many blockchain projects like to provide a different security model to explain why their blockchain is secure against each type of attack that can be done.
This strategy is misleading.

If you have a security model showing that you are secure against attack A with trust level N, and a different security model showing that you are secure against attack B with trust level N, this does not necessarily mean that you are secure against A and B at the same time with trust level N.

Each security model has a cost of enforcement, then that means enforcing both security models at once is more expensive than just enforcing one at a time.
So if you are only considering one of the security models at a time, then it is not obvious the true cost of enforcing all these different security mechanisms simultaniously.

This is why each blockchain protocol needs to have exactly one security model that takes into account all possible ways that the blockchain could be attacked. Having exactly one security model is the only way we can accurately measure the level of security of a mechanism in comparison to other mechanisms.

