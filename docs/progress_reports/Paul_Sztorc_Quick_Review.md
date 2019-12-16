Paul Sztorc
So, how do I learn how Amoveo works, anyway?
Zack
admin
https://github.com/zack-bitcoin/amoveo this is a good place to start
GitHub
zack-bitcoin/amoveo
A blockchain for trust-free markets in financial derivatives - zack-bitcoin/amoveo
Welcome Paul, thank you for joining us
Paul Sztorc
This is like in alphabetical order https://github.com/zack-bitcoin/amoveo/tree/master/docs/design
GitHub
zack-bitcoin/amoveo
A blockchain for trust-free markets in financial derivatives - zack-bitcoin/amoveo
Zack
admin
Paul invented the idea of blockchain prediction markets, Amoveo is trying to achieve the goals he spelled out for us.
https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/oracle.md
I think you would be interested in the oracle
GitHub
zack-bitcoin/amoveo
A blockchain for trust-free markets in financial derivatives - zack-bitcoin/amoveo
we are trying to re-use nakamoto consensus to power the oracle
Paul Sztorc
Zack
Paul invented the idea of blockchain prediction markets, Amoveo is trying to achieve the goals he spelled out for us.
I also gave a talk about the blockchain oracle problem and why it is so hard https://www.infoq.com/presentations/blockchain-oracle-problems
InfoQ
Blockchain: The Oracle Problems
Paul Sztorc talks about why the oracle problem is so hard (specifically, the historical evolution of failures, from API, to clearnet website, to darkn...
Zack
we are trying to re-use nakamoto consensus to power the oracle
of course there is a tension between efficiency and resolution-labor
Zack
admin
that might not be the best way to share your video, it isn't working in firefox with ubuntu
Paul Sztorc
miners do not want to have to look into any outcomes, especially difficult-to-resolve ones (especially ambiguously-worded ones)
Zack
admin
yes, I agree
we have some escalation mechanisms in place, and the nash equilibrium is that the miners will never actually have to care about any oracle
Paul Sztorc
Ah, but then why would you say that you are using nakamoto consensus for the oracle?
Zack
admin
By using nakamoto consensus as a final source of truth, it is possible to make a much cheaper design, in comparison to trying to rebuild an entire consensus mechanism
because that is what a trustless oracle is. it is a way for a community to come to consensus about some data.
Paul Sztorc
Ok so can you explain the outcome-resolution process
?
give me everything that happens in order
Zack
admin
https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/transaction_types.md
there are 3 tx types for oracles
GitHub
zack-bitcoin/amoveo
A blockchain for trust-free markets in financial derivatives - zack-bitcoin/amoveo
one for asking a question. it has to be a true/false question.
one for making a bet in the oracle. which is a way to move your money to the side of the fork that you think will have higher valued tokens.
and finally, a tx to close the oracle, if it has stayed in the same state for along enough period of time.
we combine the results of multiple oracles to encode binary data.
The turing complete smart contracts make it possible to write stuff like this.
Paul Sztorc
No, I just want everything that happens, in order
1. "ask question" message
2. "bet" message
3. "close oracle" message
Zack
admin
1) someone makes a new_oracle_tx, asking "did trump win the election?"
2) trump wins.
3) someone makes a bet in the oracle saying they would prefer owning veo on the version of the chain where trump wins.
4) after enough time passes, someone does an oracle_close_tx to close the oracle.
chronologically
that is the typical order of events.
the nash equilibrium is that only one person makes one bet in the oracle.
Paul Sztorc
You can't bet on the outcome until after it happens?
Zack
admin
when you ask the oracle the question, you can choose the block height when the oracle activates, and it becomes possible to bet in it.
Paul Sztorc
So, yes? or no?]
Zack
admin
being able to ask the question ahead of time means we can start making smart contracts that reference that oracle's id
you can't bet on the outcome until after it happens.
in the oracle
Paul Sztorc
?? Then what is the point
of having an oracle at all
Zack
admin
the oracle isn't for betting. only one person makes a bet per oracle question. and their bet isn't 'even matched
the oracle provides data, our smart contracts can access the data
Paul Sztorc
Ah, so what you mean to say is that your list is incomplete
Zack
admin
we have markets in the smart contracts that match trades in single price batches
Paul Sztorc
Zack
1) someone makes a new_oracle_tx, asking "did trump win the election?" 2) trump wins. 3) someone makes a bet in the oracle sayin
you left out a bunch of important steps for some reason
Zack
admin
I thought you wanted to just know about the oracle first
14 blocks until the hard update activates
ok, ill make a longer chronological list
Paul Sztorc
Ok so it is
1. Question asked ; appears in list
2. Users bet
3. There is a meta-market about the oracle itself; people bet on just one outcome
4. After a contest period, it is decided that whatever was bet on in 3 is what happened
Of course, there will be many people who are going to lose a ton of money. And they want to manipulate outcome-resolution
Zack
admin
1) someone makes a new_oracle_tx, asking "did trump win the election?" using this page: http://159.89.87.58:8070/new_oracle.html
2) Bob uses the same light node tool to generate a request to bet on whether trump will win. they publish it online and then turn their computer off.
3) Alice sees the trade online, uses the same light node tool to accept it. she generates a new_channel_tx to create a channel with Bob and publishes it.
4) trump wins.
5) Bob comes online, and sees that trump won. Now that he knows the result of the bet, he sends a request to Alice so they can get their money out of the channel.
6) Alice comes online, she sees the request from Bob. She signs it, and publishes it as a channel_team_close tx to the blockchain.
7) someone makes a bet in the oracle saying they would prefer owning veo on the version of the chain where trump wins.
8) after enough time passes, someone does an oracle_close_tx to close the oracle.
Paul Sztorc
What does the person in 7 get, in return for their message?
Zack
admin
If you bet on the wrong outcome in the oracle, then that is the same as moving your money to the side of the fork that will be less valuable.
So everyone will want to bet against you to own more money on the side of the fork that is more valuable.
Paul Sztorc
What does the person in 7 get, in return for their message?
since Alice and Bob already closed their channel, it doesn't actually matter to them how the oracle is actually settled.
If Alice refused to cooperate in closing the channel, then the result in the oracle would be important to Bob.
So he would pay the $2 minimum bet to make the oracle close correctly, and then the smart contract could execute and give the money to Bob.
it is profitable to make an accurate bet, so Bob could just post on our forum that it is ready to be closed, and probably someone else will do it.
Especially once I add that to the browser light node.
https://github.com/zack-bitcoin/amoveo/blob/master/docs/white_paper.md
The white paper gives a good overview, and has more coherent links to other places in the docs
GitHub
zack-bitcoin/amoveo
A blockchain for trust-free markets in financial derivatives - zack-bitcoin/amoveo
Paul Sztorc
Zack
1) someone makes a new_oracle_tx, asking "did trump win the election?" using this page: http://159.89.87.58:8070/new_oracle.htm
Let's stress test it

1) Alice makes "did trump win the election" question
2) Bob bets Alice $10,000,000 that trump will win.
3) Alice accepts.
4) Trump wins, meaning that Alice is about to be $20,000,000 poorer than she thought. She will not only lose 10M but will fail to win 10M.
5) Bob tells Alice to surrender the 20M worth of value...
6) ...but Alice ignores Bob.
7) Alice bets $2 in the oracle that Trump actually didnt win.
...(something happens)...
Zack
admin
in that case, Bob can come to the forum, tell us how much money Alice is willing to bet, point to the oracle question, and we will all bet against her.

The market cap is only like $4 million though
Paul Sztorc
Imagine the market cap is 4 trillion
Zack
admin
if Alice has more money than the rest of us put together, then by default her side of the oracle would win
Paul Sztorc
Bob comes to the forum and other people bet on the the trump=win oracle market
Zack
admin
So in that case, we would have a week to set up a soft fork that will censor any block that closes the oracle the way we think is a lie.
So then we all stay on the side of the fork where we get rich from Alice.
Paul Sztorc
when does it close
Zack
admin
If alice has more money than everyone else put together, it closes on both sides after the fork
if alice has less money, then it never forks.
and she loses all her money when we close it
Paul Sztorc
the 20M stays trapped forever
?
Zack
admin
in no case does the money get trapped forever
Paul Sztorc
What happens if some people bet in the oracle market trump=win and others bet trump=lose?
what happens next?
Zack
admin
oh, if the blockchain forks, then all the accounts and channels exist on both sides.
So Bob would win the channel on the side with valuable tokens, and Alice would win on the side with worthless tokens
Sometimes the community forks, and it makes sense for the blockchain to split so we can each achieve our different dreams. in that case, the oracle mechanism is a way to move your money to the side of the fork that you think will be more valuable.
But it is not very efficient, it only matches bets at a price of 1:1.
Paul Sztorc
Zack
oh, if the blockchain forks, then all the accounts and channels exist on both sides. So Bob would win the channel on the side wi
Yes we all agree that can be used as a last resort, in a very limited scale...maybe once every few months for very obvious questions
Zack
admin
my expectation is that it will never happen.
Paul Sztorc
Ah see
Zack
admin
No one will be willing to throw so much money at the system.
Paul Sztorc
So the oracle-market-bets
Zack
admin
but we still need to be ready. if we weren't ready, then they would attack.
Paul Sztorc
Are pledges, and the people going against you are by definition sacrificing their money on that side of the chain
Zack
admin
yes, it is a way to try and reuse nakamoto consensus for making a binary decision
Paul Sztorc
But you said you were only using nakamoto consensus as a last resort? You seem to be using it basically as a first resort
Zack
admin
the expected case is that one person makes one bet in the oracle, and no fork occurs.
Paul Sztorc
So the blockchain splits into three ("yes" "no" "ambiguous") for each semi-ambiguous question, though
?
Zack
admin
yes, there are 3 possible outcomes.
The smart contracts can which of the three an oracle finalized in, and you can program the contract to do whatever you think is right.
Paul Sztorc
Zack
the expected case is that one person makes one bet in the oracle, and no fork occurs.
And you did not answer my question as to what this person receives. Anything?
Zack
admin
since bets are matched in a price of 1:1 in the oracle, we only store one kind of bet in the order book at a time.
Paul Sztorc
And you did not answer my question as to what this person receives. Anything?
there is a cost to asking the oracle a question. this fee is used to make the very last bet in the oracle, and it is always a bet for the losing side.
This gives incentive for the first person to make that one bet.
Paul Sztorc
Ah I see
An incentive for them to make some bet, not necessarily the winning bet. They try to make a winning bet to avoid a fork
When someone buys Amoveo for the first time, they must personally research all of the past events, however. If there are many forks
Zack
admin
it is better to own veo on the side of the fork that is more honest, because the honest side will have more valuable coins.
Paul Sztorc
That is a problem if some of the forks are from questions in a different language
Zack
admin
Most people just use the light node
Paul Sztorc
or which are generally difficult to answer
Zackadmin
Paul Sztorc
That is a problem if some of the forks are from questions in a different language
yeah, dealing with different languages could get tough, we haven't thought much about that.
Paul Sztorc
When someone buys Amoveo for the first time, they must personally research all of the past events, however. If there are many fo
miners can only mine if the tokens are valuable.
Paul Sztorc
Well if I were going to lose 20M, I would try to make my fork do well
At least the trump=ambiguous fork
Especially if it were a multivariate market, if the crypto-price collapsed it wouldn't affect my winnings
Zack
admin
if you already lost 20M, you would throw more money into the oracle and launch a fork of a cryptocurrency?
Paul Sztorc
Zack
if you already lost 20M, you would throw more money into the oracle and launch a fork of a cryptocurrency?
If my fork wins, I un-lose the 20M.
Zack
admin
Would you hire developers to run the github and maintain the software?
Launch a mining pool?
Paul Sztorc
I would just copy the development / software of the most popular rival team
Zack
if you already lost 20M, you would throw more money into the oracle and launch a fork of a cryptocurrency?
The fork only costs $2, not $20M
Zack
admin
If you are willing to go through all that, it isn't much different from ICO's giving out free money to competitors
Paul Sztorc
The 20M is in the channel in any case
Zack
admin
you can fork any blockchain from any point in history
Paul Sztorc
I would just be spending $2 and saying that the Trump question was actually ambiguous
Zack
admin
if you had 20M in bitcoin, and sold it. you could decide to fork from before when you sold those bitcoins
someone would bet more than $2 against you that he lost.
Bob told us that it is worth $20 million to you. we are like sharks smelling blood, hoping you will throw more cash at us.
the order book will fill up, because the oracle's order book is first come first serve.
you can't pay more to get ahead, because there is only 1 price.
so then you wont be able to change the oracle to ambiguous again so easily.
Paul Sztorc
What happens after I put $2 on trump=ambiguous ?
Zack
admin
someone else puts $4 on trump = true
Paul Sztorc
Someone else puts $2 on trump=win ?
ok
Zack
admin
right
Paul Sztorc
When does the blockchain fork?
Zack
admin
or more
they might put $1000 on trump=win immediately, because of the first come first serve aspect of the oracle bets
the nash equilibrium is that it would never fork
Paul Sztorc
No I'm trying to get it to fork though
Zack
admin
if you had more money than everyone willing to bet against you, and you kept betting on the lying outcome
then eventually we would reach a point where we have 7 days to set up a soft fork to censor any blocks that would close the oracle in the lying state.
Paul Sztorc
??
Zack
admin
So then you would have to convince some mining pool to run your version of the software without the soft fork, so you can close the oracle on your side the way you think is right
Paul Sztorc
Ok, but every time there is a semi-ambiguous outcome, the blockchain *will* fork
that is an outcome where it is objectively unclear if the true outcome should be "true" or "ambiguous" (or "false"-or-"ambiguous")
So there would eventually be thousands of blockchains, seemingly
Meaning that lots of the outcomes would not really be resolved at all
Zack
admin
I think in practice we will find solutions to overcome these rare edge cases.
A fork hurts almost everyone. We have an incentive to stick together, and we have futarchy to help us decide.
Paul Sztorc
But as I have explained, it isn't rare at all
And it is already happening to Augur
An attacker can just make many semi-ambiguous questions
Zack
admin
if is objectively ambiguous, then we can decide with a coin flip
there is a lot of incentive not to fork, and futarchy is powerful.
Paul Sztorc
You could, but your oracle mechanism would then be a coin flip
and you'd need some way of knowing what the coin's outcome was
and why people should agree to what it says
Zack
admin
for a financial derivative to be useful, it needs to approximate a certain risk profile
as long as the approximation is near, then the user is happy.
if it is a close call, then the decision is not important
Paul Sztorc
But if there are 1,000 forks of Amoveo, each with a different market cap
you are ok with that?
The user will have a hard time knowing which one to buy
and the hashrate will be split each time
Zack
admin
I think that this oracle mechanism holds the potential to be very cheap.
I doubt it will fork the way you imagine, but if it does, we will adapt.
Paul Sztorc
That is just denying the existence of semi-ambiguous questions
even though they do exist and have already been used against Augur
Tarrence van As
Why would someone bet on an intentionally ambiguous question? If no one bets, then the stakes are low and the outcome doesn't matter. Seems there will only be forks if the stakes are high enough
Paul Sztorc
Hopefully they would not
bet
but it seems already your betting is taking place in channels?
Zack
admin
over time we will develop a culture to filter questions more and more finely into the 3 types. like how the legal code grows
Paul Sztorc
So you wouldn't be able to tell high-bet questions from low-bet questions
So it seems that that distinction couldn't matter
Zack
admin
I sold some USD stablecoins recently
for a 1 month contract
Paul Sztorc
Zack
over time we will develop a culture to filter questions more and more finely into the 3 types. like how the legal code grows
culture does require some kind of meta-culture to enforce it
but I am suggesting that an attacker could spam the system
intentionally inserting many semi-ambiguous questions
Zack
admin
In Amoveo there is no obligation for anyone to report to an oracle. This makes it easier for us to ignore the meaning of the words, and follow the spirit of what is useful.
if they spam questions, we can just ignore them entirely. if no one bets, the results of an oracle don't matter.
Jon Snow
Wow, the lengary Paul is here. Welcome
Tarrence van As
Protocols can be developed to help pose well formed questions. Like veil is doing ontop of augur today. Questions can be "endorsed" by reputable parties. Those reputable parties can run markets and earn fees for providing their service
Today it is very immature though
Zack
admin
In our scalar markets. we use 10 bits to encode the price of bitcoin, and each bit is an oracle.
So the least significant bit is almost always ambiguous.

But the least significant bit also has the least influence on the outcome of the contract. So it isn't likely to create an incentive for a fork to occur.
Paul Sztorc
So the oracle model is entirely hashrate-dependent, though
not that that's the worst thing
but basically you are assuming everything will be uncontested
and if something is contested it forks the chain
Zack
admin
im not sure what "hashrate dependent" means
Paul Sztorc
and investors need to decide which version of amoveo they'd rather support
investors will have a strong incentive to support whichever version they think other people are supporting, though
hashrate-dependent means "miners decide"
Zack
admin
nakamoto consensus fundamentally supports forking, and it is also possible to sell bitcoin, and build a fork from before you sold them.
It seems like these same risks exist in bitcoin as well.
Paul Sztorc
I dont agree
Zack
admin
miners have to mine the version that has coins valuable enough to sell for electricity
Paul Sztorc
in Bitcoin you can ignore all forks
Zack
admin
investors decide
Paul Sztorc
The requirement that hodlers research all outcomes and validate them makes it much more annoying to run a node
Zack
admin
if someone undoes 3 blocks of bitcoin quickly enough, all the full nodes will follow them and you wont even notice that 3 blocks were undone
Paul Sztorc
that is why I outsourced the task to VoteCoin-ers
Zack
admin
in Amoveo we outsource it to the oracle reporters.  they escalate the decision to the point where miners can profitably get involved.
Paul Sztorc
not really though
because it is ultimately outsourced to hodlers
when the blockchains fork
Zack
admin
if the situation never escalates, then that means not enough money was at stake in that oracle for it to matter
if the situation does escalate, then hodlers and miners can profitably figure out what is going on and resolve it.
mining pools run the software, not the miners.
miners can only choose which pool to use.
Paul Sztorc
yes but in Amoveo, every users is expected to resolve all oracle outcomes
so that they know which fork to back, if the network splits
Zack
admin
only the ones that have escalated to the point where it is profitable to get involved.
Paul Sztorc
that is the only way that it becomes true that "no forking happens" becomes a nash equilibirium
Zack
admin
Well, I guess we will find out soon if Amoveo will die in forks like you predict
Paul Sztorc
no I instead predict that running a node will be very expensive
but either way
Zack
admin
its pretty cheap so far
Paul Sztorc
zzzzzz
As I said
Zack
admin
why would it be expensive to run a node?
Paul Sztorc
because it includes the labor of personally checking every oracle outcome
even ones that you know nothing about, don't care about or are written in differnet languages
Zack
admin
http://159.89.87.58:8070/txs.html just click this link, and you have synced with the network
It isn't so useful to end up on a fork by yourself. users will chose a fork that is being maintained by developers and has active mining pools.
they will have a full node software that syncs with the version they want.
Paul Sztorc
yes but deciding what they want
is very expensive in this case
You seem to want something both ways
Zack
admin
there is an incentive for us all to stay on the same fork. trustless coin flip protocols are easy to write.
Paul Sztorc
— people will know every outcome and enforce it, and people will be lazy and just click your hyperlink to find out what happened
Zack
admin
If the question is ambiguous, then it shouldn't matter how it resolves. as long as we can all agree.
Paul Sztorc
Interesting
ok, well thanks for explaining it to me :+1:
Zack
admin
feel free to ask questions about any part any time
We are big fans of you here