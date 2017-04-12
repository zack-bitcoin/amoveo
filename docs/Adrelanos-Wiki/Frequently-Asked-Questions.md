# How is Aeternity different from Ethereum? #

On Ethereum contracts exist on-chain for multiple blocks. They hold state and can interact with other contracts.
On Aeternity each contract only exists for a moment. It is settled independently of all the other contracts.

# Why is Aeternity faster than Ethereum? #
Since contracts on Aeternity are independent, they can be processed in parallel.

On Ethereum it is possible to move computation, but it is much more complicated than standard contracts. A lot of boilerplate code needs to be reimplemented for every contract.

On Aeternity computation by default happens off-chain.

# How does Aeternity protect from the next DAO happening? #
Since contracts are independent, it is much easier to prove and verify what each contract does.

The DAO was a problem where some people trusted other people to spend their money for them.
If you give your money to someone, and trust them to spend it for you wisely, there is nothing that I can do to stop them from robbing you.

Aeternity supports many trustless contracts. There is no reason you should ever use trust with Aeternity, or any other blockchain system.
Any smart contract that requires trust is not so smart.

# Is smart contract verification on the roadmap? #

The language for Aeternity is very simple. Smart contracts do not depend on each other's state, so it is easy to prove the correctness of a smart contract.
We wont need any special verification software to be sure of the correctness of Aeternity contracts.

Channels can be connected to each other using hashlocking. This is how cross chain atomic swaps work. This is how the lightning network works. This is how we can build prediction markets with more than 2 users.