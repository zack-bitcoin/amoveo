eternity
aeternity is a blockchain designed for fast and efficient smart contract processing.
aeternity has developed an innovative approach to scalability with an oracle. Based
on our approach to storing Turing-complete contract code
in state channels rather than on-chain, we have been able to make smart contracts easier to
analyze and faster to process. This increases scalability since all
transactions become independent and can thus be processed in parallel. Additionally, this
means that contracts never write to shared state, greatly simplifying their testing and verification.
aeternity will be powered by Aeons, the token.
Aeons are used as payment for any resources one consumes on the platform, as well as the
basis for financial applications implemented on the platform. The distribution of aeons in the
genesis block will be determined by a smart contract hosted on Ethereum.
Aeons will be managed by accounts which will have an address and balance, implemented
alongside a naming system which will support human-friendly names mapped to an aeternity
address.
aeternity’s blockchain:
Each block will contain each of the following components:
• The hash of the previous block.
• A Merkle tree of transactions.
• A Merkle tree of accounts.
• A Merkle tree of names.
• A Merkle tree of channels.
• A Merkle tree of oracles which haven’t answered their respective questions.
• A Merkle tree of oracle answers.
• A Merkle tree of Merkle roots, used for identity and proof of existence.

State Channels
Aeternity’s blockchain leverages state channels which ensures only those affected by the
transaction need to know about it.
1. First, two users make an agreement on the chain to form a channel relationship.
2. They can agree to smart contracts written off-chain. In the event of disagreement,
the blockchain can 

2. The parties then send signed updates to the state between each other. This allows for
transactions to be conducted as fast as information can be transmitted and processed by
the parties, instead of them having to wait until the transaction has been validated—and
potentially finalized— by the blockchain’s consensus mechanism.
3. This aeternity blockchain is used to settle the final outcome or to resolve conflicts that
arise.a.
It’s decisions are foreseeable, thereby ruling out malicious activity between
parties who try to erode the state channels to which they’ve written and
previously agreed to.
The transfer of aeons is the only method by which a state update that can be settled on the
blockchain. And only those aeons that have been deposited into the state channel can be
transferred.
This makes all channels independent from each other, which has the immediate benefit that any
transactions related to channels can be processed in parallel, greatly improving transaction
throughput.
Smart Contracts
aeternity will use a Turing-complete virtual machine to process smart contracts. Within aeternity,
smart contracts are pure functions in the form of financial agreements that distribute funds
according to predetermined rules.
- Only the involved parties know about the contract
- Only parties that have an open state channel can create a valid contract
- It is only submitted to the blockchain if its outcome is disputed, in which case the code is
only ever stored as part of the submitted transaction, never in any other state.
- If this happens, the blockchain distributes the tokens according to the contract
and closes the channel.
Implementation
Our core blockchain code is written in Erlang, which makes it easy to write distributed,
fault-tolerant, soft real-time, highly available, non-stop applications. Erlang is the perfect
language to write a blockchain from scratch and by using it, we achieve a superior operational
stability and performance.
Consensus
Proof of Work blockchain
Blocktime
10 minute (current testnet)
Development Roadmap
Test net - launched
Launch of 1st app - date tba
Main net launch - date tbaTeamLinks to social media:
Website: http://www.aeternity.com/
Facebook: https://www.facebook.com/aeternityproject/
Twitter: https://twitter.com/aetrnty
Linkedin: https://www.linkedin.com/company/aeternity
Reddit: ​ http://reddit.com/r/aeternity