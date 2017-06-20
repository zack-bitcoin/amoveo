Using cowboy server. test done on June 20 2017. Using a node in london, and a node in amsterdam.

These servers each cost about $5 a month.

one node made 20 requests of the other, asking for 2k bytes each time.
This took 0.85 seconds.

a lightning payment has nine data transfers.
if alice pays carol through bob:

alice                          bob                        carol
  --hashlocked update --------->
  <----- hashlocked update -----
                                 <----request channel data---
				 -- hashlocked update ------>
				 <----secret+unlocked update--
				 --- unlocked update ------->
  --request channel data ----->
  <-- unlocked update ---------
  ---- unlocked update ------->

curl -i -d '["header", 0]' http://localhost:8040
so each sequential lightning payment made by one node takes about 0.3825 seconds. If you wanted to make 100 lightning payments, it would take at least 38.25 seconds.

These requests can also happen in parallel.
curl -o /dev/null -i -d '["test", 2000]' -s -w %{time_total}\\n  http://178.62.112.217:3010 &

If a node is processing lightning transactions in parallel, each payment takes about 0.03 seconds. If you wanted to make 100 lightning payments from a single computer, it would take at least 3 seconds.


30 payments per second for a month makes 7.77*10^7 transactions per month.
So $0.01 can pay for the hardware and internet service necessary to make 155,520 lightning payments.