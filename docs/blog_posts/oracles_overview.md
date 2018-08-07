Based on discussions between Zack and Mr Flintstone.

How does the Amoveo oracle work?

Oracles help us record the correct outcome of certain real world events on the blockchain in a trust minimized way. Users can ask an oracle a true/false question and rely on the fact that it will answer truthfully, provided a well structured question. If users can rely on this fact, they will be comfortable to attach financial derivatives to future answers via a smart contract.

In Amoveo, there are two kinds of oracles. Question oracles function as described above and answer true/false questions about real world events that happened in the past. Governance oracles determine the future values of Amoveo’s governance variables (block size, block time, block reward, etc) by first specifying a percent change then asking the oracle if the governance variable should increase by that change, decrease by that change or not change at all. While the applications are different, these two oracles actually operate in the exact same way, for the most part. We will walk through the process of a question oracle below.

We can ask the oracle a true/false question using the following amoveo-core node command.

	api:new_question_oracle(Start, Question).

We will describe how the oracle answers this question in two ways. The next paragraph is an abstract overview and the one after that is a concrete example.

“Start” is the block height at which the oracle starts. Once the oracle starts, users can place bets denominated in VEO on true, false, or bad question based on what the question was. We call true, false and bad question types. Bets of different types will get matched on a 1:1 basis. The type with the highest unmatched balance determines the output type of the oracle. If the output type of an oracle has not been changed for a certain number of blocks, the oracle can be finalized with that output type and betting ends. Placed bets are locked up until the oracle is finalized, at which point you are paid out according to the final output type. An example of this is below.

For example, an Oracle starts. This means betting has started. The question is “did Amoveo mainnet survive for a month since the Amoveo mainnet genesis block’s timestamp?” We know that the answer to this question is true. Say I bet 10 VEO on true and you bet 5 VEO on false. Now, 5 has matched for both of us at 1:1 odds, and there is 5 unmatched remaining of the type true. The 5 unmatched of type true determines that the oracle now has output type true. If the output type doesn’t change for a certain number of blocks, the oracle is finalized in this state. Next, because the final state was true and I bet on true, I win my matched bet worth 10 and get the 5 that was unmatched back for a total of 15. You lose your matched bet. Any financial derivatives attached to this outcome pay out accordingly.

As you can see, the proper functioning of question oracles in Amoveo relies on VEO holders’ willingness to bet against incorrect bets. This willingness relies on the assumption that miners will follow the chain where the final oracle types are true representations of what happened. This is the same kind of consensus Bitcoin uses. As long as holders can rely on this fact, they will be comfortable to take the other side of any incorrect oracle bets. Also, if this assumption holds, nobody would even want to bet on the incorrect outcome because they would definitely lose money

If the oracle is about to be closed the wrong way, then you can double your money by making a bet for it to close the right way

If the attacker keeps trying to bet the wrong way, his bets become a reward attracting more people to bet the right way.

Once enough money is at stake, then it becomes worth it for miners to manually check the outcome. Since the miners can double all their money.
