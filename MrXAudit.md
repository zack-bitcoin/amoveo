> mr:
Amoveo codebase security audit — 2026-08-14
Scope: github.com/zack-bitcoin/amoveo master (131 Erlang modules) + the zack-bitcoin/verkle dependency. Method: five parallel subsystem auditors (transactions, trees/proofs, chain/block-production, networking/HTTP, economics), each finding independently re-verified against source by reading the exact cited code. Live chain height ~421,360; all forks up to 55 active.
Confidence tags: CONFIRMED = traced end-to-end in source here. SUSPECTED = mechanism verified in-module but full reachability depends on a component not exercised.
The linchpin (read this first)
Most of the severe findings share ONE root defect:
block:make/4 applies the entire mempool as a batch through one shared state dict, with no try/catch and no skip-the-bad-tx logic (block.erl:458 → txs:digest → txs.erl:16-39). Any transaction that throws while being applied aborts the whole block build. That crash kills the potential_block gen_server, whose terminate/2 (potential_block.erl:19-27) dumps the mempool and re-injects the txs 2s later — an infinite crash-loop — while the supervisor's 50000/sec restart tolerance (amoveo_sup.erl:62) hides it. Because mempool ADMISSION validates each tx alone (tx_pool_feeder.erl:468, txs:digest([SignedTx], OldDict, ...)) while block-BUILD validates the batch, a tx can be accepted, gossiped network-wide (sync.erl trade_txs), and then halt block production on every node that holds it. Fix the isolation (wrap each tx application, drop the offender, keep building) and an entire class of halt/DoS bugs — including the one freezing the user's own transactions — collapses at once.
Everything tagged [linchpin-trigger] below is a distinct way to fire this same gun.
CRITICAL
C1 — One cheap transaction permanently hangs every node (chain halt)
trees/jobs.erl:41-52. rat_exponent(N,D,E) has a base case only for E==1; E==0 satisfies the even-exponent guard and recurses on 0 div 2 == 0 forever — a 100% CPU infinite loop (not a crash, so no restart saves it). Reachable from salary_update (jobs.erl:107-144) whenever Blocks2 or Blocks computes to 0, via job_receive_salary_tx / job_buy_tx / job_adjust_tx / job_team_adjust_tx (all live since fork 53). A ~0.00002 VEO job + one salary tx wedges the tx_pool_feeder of every node that absorbs it, and every validator of a block containing it. CONFIRMED — found independently by two auditors; non-termination verified in source. Highest-severity finding. Fix: base cases for E =< 0, and guard Blocks/Blocks2 > 0.
C2 — No per-transaction isolation in block production [linchpin]
chain/block.erl:458, consensus/txs.erl:16-39, chain/potential_block.erl:19-27,44-47,132. See linchpin above. Any throw in any tx's go/4 halts the whole block; crash-loop + mempool-flush + re-gossip make it stick and spread. CONFIRMED. Fix: isolate each tx apply; on failure drop that tx and continue; never let one tx abort the block.
C3 — Account-creation is broken chain-wide (this is the user's stuck-VEO bug) [linchpin-trigger]
trees/trees2.erl:997 (get_proof) vs trees2.erl:920 (get/2); reached via verkle/get_verkle.erl:652-656 (points_values). The verkle layer returns a neighbor leaf (Type 2) for a key that routes to a lone-leaf position, without checking the leaf's key equals the requested key. get/2 (mempool path) guards this with not(Key == Key2) and yields empty; the proof path get_proof (block-build path) does NOT — it deserializes the neighbor account as the new key's value. So accounts:dict_get(Pub,...) returns a real #acc{}, and create_account_tx.erl:30's empty = accounts:dict_get(...) badmatches → block halts (via C2). This is why every create_acc_tx sits accepted in all mempools yet never confirms. CONFIRMED code asymmetry + reachable mechanism. (Corrects the earlier "absence proofs aren't generated" theory — the real defect is the missing key-guard on the proof path.) Fix: add the Key == Key2 guard to get_proof, emitting the absence fact on mismatch.
C4 — Unauthenticated, self-propagating network halt [linchpin-trigger]

> mr:
tx_pool_feeder.erl:468 (admission ≠ apply) + sync.erl trade_txs + C2. Two create_acc_tx for the same new pubkey from two funded senders each pass admission (pubkey absent from confirmed state), gossip to all peers, and then batch-crash block:make (tx1 creates the key, tx2 sees it exists → badmatch) on every miner. Survives a fix to C3. Cheap, unauthenticated, persistent. CONFIRMED. Fix: C2 (isolation) neutralizes it; ideally also make admission mirror block-apply semantics.
HIGH
H1 — Subcurrency theft via negative amount
txs/sub_spend_tx.erl:10-46. go/4 never checks amount >= 0 (unlike spend_tx/create_account_tx). Line 35 credits sender -A, line 44 debits recipient +A; a negative A reverses the transfer, letting the signer drain any victim's token balance (victim must hold ≥ |A|; attacker pays only the VEO fee, and only the attacker signs). CONFIRMED. Base VEO is unaffected; the subcurrency system is. Fix: true = A >= 0.
H2 — ["blocks",Many,N] reads the whole chain uncompressed into one process
amoveo_http/ext_handler.erl:128-133 → block_db3:read/2. No cap on Many (the streaming /blocks/N_M path caps at 1002; this RPC does not). ["blocks",99999999,0] inflates + decodes ~421k blocks into a single list inside the block_db3 gen_server that all reads/writes/mining depend on. CONFIRMED — unauthenticated node-wide stall + likely OOM. Fix: cap Many.
H3 — ["peers",List] → SSRF / reflected connection flood / FD exhaustion
ext_handler.erl:181-183 → peers:add/1 → talker:talk (httpc with 120s timeout, attacker-controlled IP:port, no list-length cap). One request makes the node open outbound connections to arbitrary hosts/ports. CONFIRMED. Fix: cap list length; restrict/validate targets.
MEDIUM
M1 — Mempool flush drops valid txs every block. chain/block_db3.erl:352-353: absorb_dump2(Block, []) rebuilds the pool empty and the following tx_reserver:restore() is commented out. Unincluded honest txs are silently dropped on every block. CONFIRMED. (This is the "mempool flush" behavior observed operationally.)
M2 — empty = <tree>:dict_get badmatch family [linchpin-trigger]. Same crash class as C3 in oracle_new_tx.erl:90, contract_new_tx.erl:30, stablecoin_new_tx.erl:60/67, job_create_tx.erl:51, swap_tx.erl:78. Any of these creation txs can halt a block build the way create_acc does. SUSPECTED (in-module confirmed; shares C3's proof-path reachability). Fix: C2 + C3.
M3 — market_swap_tx crashes the block on slippage / zero-reserve [linchpin-trigger]. txs/market_swap_tx.erl:107,120 use 1 = 1+1 (deliberate badmatch) for the slippage-fail path instead of a clean reject, and :99/:112 can badarith divide-by-zero if a prior same-block swap zeroes a reserve. Two swaps on one market, both admitted, crash the template. CONFIRMED mechanism. Fix: C2 + clean rejects.
M4 — tx_pool_feeder blocks ~2s per tx. ext_handler.erl:195-204 + feeder receive ... after 2000. A tx that passes signature+fee but throws in digest yields no reply → full 2s stall of the single tx-intake gen_server. A few IPs saturate all tx intake. CONFIRMED.
M5 — Deleted account/subcurrency balances may resurrect. trees/csc.erl:47-50 remove keeps the old val; chain/tree_data.erl:204-206 re-serializes surviving dict values back into the verkle tree ("we never delete from the verkle tree"). A delete_acc_tx/sub-account delete may leave the pre-delete balance in the committed tree. SUSPECTED (re-write confirmed; exact retained value needs a live delete trace).
M6 — ["proof",IDs,Hash] unbounded verkle proof generation. ext_handler.erl:340-347: no length cap on IDs (contrast sub_accounts which caps at 1000). CPU/bandwidth amplification. SUSPECTED.
M7 — ["headers",Many,N] = up to 5000 serial gen_server reads. ext_handler.erl:152-154. Saturates the headers process that mining/sync depend on. CONFIRMED.

> mr:
M8 — stablecoins tree bypasses the csc consensus-state layer. trees/stablecoins.erl:157-169 use raw dict:find/dict:store for get/write (while dict_delete uses csc:remove). Post-verkle the commit path iterates csc entries, so stablecoin objects may never commit to the tree/proof set. No VEO is tracked in them, so it's a consensus-consistency concern, not a money bug. SUSPECTED.
M9 — Received-block path has no try/catch. chain/block_db3.erl:419-424 (absorb → block:check0/check2). A crafted/poison block crashes the sync/handler process rather than being cleanly rejected. CONFIRMED (no try/catch); exploitability SUSPECTED.
LOW
L1 — Coin-conservation burn side unenforced. block.erl:1915-1943: no_counterfeit only warns (never rejects) when a block burns MORE than the 0.5 VEO bound; caller enforces only Diff =< 0. Permits excess burning, not creation — not attacker-profitable. CONFIRMED-logic (both auditors agree low).
L2 — Supervisor hides crash-loops. amoveo_sup.erl:62 {one_for_one, 50000, 1} tolerates 50k restarts/sec, so C-class crash-loops never escalate or alert. CONFIRMED.
L3 — Header validation rejects via crash; only an upper timestamp bound. chain/headers.erl:493 (true = Time < now+20, badmatch) and 1=2-style rejects at :185/198/207/223; no lower-bound/monotonicity check → mild difficulty-retarget timestamp gaming. SUSPECTED.
L4 — Unauthenticated introspection. ext_handler.erl:468-482: ["status",1|2] returns live memory, gen_server queue lengths, heap/stack — telemetry that helps tune the DoS vectors above. CONFIRMED.
L5 — tree_data map is partial over leaf shapes. tree_data.erl:204-206: fun(#consensus_state{val=X}) -> X; ({unmatched_head,...}) -> Y end; an unexpected surviving leaf (e.g. val=undefined) throws function_clause during root computation. SUSPECTED.
L6 — ["give_block",Block] verifies synchronously in the request handler. ext_handler.erl:105-116. Per-request verification CPU; isolated failure, gated by sync_mode. CONFIRMED, low impact.
Verified SOUND (checked and cleared — no bug)
VEO money-conservation is solid. Coinbase reward + developer reward are the only creation path and are independently re-derived and enforced by no_counterfeit (block.erl:1884-1915); Diff =< 0 enforced. No inflation/counterfeit path found.
Constant-product AMM math (market_swap_tx.erl, market_liquidity_tx.erl): output/fee arithmetic conserves VEO; rounding truncates in the pool's favor both directions (no skim); zero-liquidity div-by-zero blocked by the fork-42 amount1/amount2 > 0 invariant.
Flash-loan payback (multi_tx.erl): fully enforced; the fork-40 zero-fee fix is complete (fee paid last from real balance).
Oracle "bad question" resolution (unmatched.erl, oracle_close_tx.erl): fork-46 fix present and correct; oracle VEO conserved.
Nonce/replay: strictly-increasing nonce enforced (accounts.erl:42); replay blocked.
Integer overflow: Erlang bignums; not reachable given conservation.
market_new_tx negative amount: blocked (fork-42, markets.erl:104-106).
Governance bounds: post-fork-52 consensus values are hard-coded, so tree-value truncation is inert.
Futarchy/LMSR: disabled on mainnet (fork 54 ≈ 1e18). Would need re-audit if ever activated.
Recommended fix order
C2 (per-tx isolation in block:make) — single highest-leverage fix; neutralizes C3, C4, M2, M3 and stops the chain-wide block-production fragility. Also unblocks the user's pending VEO once deployed on mining nodes.
C1 (rat_exponent base cases) — trivial patch, prevents a permanent unauthenticated chain halt.
H1 (sub_spend amount check) — one line, stops token theft.
C3 (get_proof key-guard) — fixes account creation at the source (belt-and-suspenders with C2).
H2/H3/M4/M6/M7 (bound the remote RPC surface) — caps + validation on blocks/peers/proof/headers; move heavy work out of shared gen_servers.
The Medium/Low correctness items (M1, M5, M8, M9, L-series) as follow-ups.
Caveats
The verkle crypto internals (verify_verkle:proof, multiproof) were read structurally but not formally verified; no bypass found, but a dedicated crypto review is warranted.

> mr:
Reachability tags marked SUSPECTED depend on live trie/verkle state that can't be reproduced from source alone.
This audit reflects master; live nodes may run a different release. Consensus rules match (forks ≤55 active at the live height), but node-side behavior could differ.
