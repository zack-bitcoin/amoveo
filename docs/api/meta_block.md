meta block
=======

Configuration
=======

in the config file /config/sys.config/tmpl there is the variables "block_meta". if you set this to "true", then your full node will store meta data along with each block.
The meta data is stored in JSON format with keys and values, so it is easy to know what the different parts of the data signify.

There are many parts to the meta data. You can turn on the different parts in your config file.
```
    {block_meta_block, true},
    {block_meta_txs, true},
    {block_meta_governance, true},
    {block_meta_before, true},
    {block_meta_following, true}
```

if you turn on "block_meta_following", then the meta data will include the "following" section, which explains the resulting state of every account, channel, oracle, etc used in this block after the block is finished processing.
the "block_meta_before" is similar, but it explains the state before we start processing the block.
"block_meta_governance" tells all the governance variables, even ones not used in this block.

Accessing the meta data
=========

The meta data is stored inside each block, so any way of accessing the blocks will also give you access to the meta data.

The meta data is the 14th element of the block, and you will need to decode it base64 in order to read it.

in erlang, to get the meta data of block Block:
`element(15, Block).`
or, if you are programming into a file where the record is loaded: `Block#block.meta.`
if you load a block into javascript, you can get the meta data like this: `JSON.parse(atob(block[14])).`


There are a couple different API requests that you can use to get the blocks:

to get a single block in JSON format (warning, this is slow):
`curl -i -d '["block", height]' http://localhost:8080`

to get a list of many blocks in JSON format. This is much faster:
`curl -i -d '["blocks", start, end]' http://localhost:8081`
For very recent blocks, this request will give you ever block from height start until height end in a list.
For older blocks, this request will give you a list of around 200-800 blocks, and the list will include block height start.


Gotchas
========

The account_delete tx type isn't handled perfectly by meta. Sometimes it records the wrong quantity of veo being transfered. These are the block heights that contain an account delete tx type, as well as an additional tx that uses the same account. So these blocks might be recording account_delete incorrectly.

10724
10740
14270
14304
14321
14338
14375
14397
14433
14464

Example
=====

here is an example of a block meta data.
```
{"block":{"height":67813,"developer_reward":3256696,"block_reward":16291627,"diff":13847,"prev_hash":"4Sd1sRblqjVNNMOPmJyJnuPvl3aLtDJh78CbCBas8BA=","blockhash":"YGdOo+Mz+hDSkR7rf2VeKt/wFqT8mY9kj0XaGiK3Eo8=","time":400339705,"market_cap":6528777307400},
"txs":[{"type":"coinbase","txid":"UJBgaGlj1YX+iQIN6DX8JHILWHOJiubyxaelgJ+YXdM=","to":"BC80oG/EAXojuLCjIjQmIQgTv9wHscgCccEy4q7R2Vwak2iPbrTb1htgVOU+NjChxxmOeNiUJMxURPqUEWZ2lzc="}],
"governance":{"oracle_question_liquidity":2210247,"oracle_winnings":151118,"unmatched":151118,"oracle_close":151118,"oracle_bet":151118,"oracle_new":151118,"ex":151118,"cs":151118,"timeout":151118,"csc":151118,"ctc":151118,"nc":151118,"delete_acc_tx":0,"spend":60657,"create_acc_tx":151118,"maximum_question_size":999,"maximum_oracle_time":3992,"minimum_oracle_time":999,"oracle_initial_liquidity":33826071,"governance_change_limit":51,"var_limit":9481,"fun_limit":980,"space_gas":1004015,"time_gas":1004015,"block_period":5982,"max_block_size":132114,"developer_reward":0.1999,"block_reward":16291627},
"before":[{"type":"gov","id":"block_reward","value":1420,"lock":0},{"type":"account","pubkey":"BL0SzhkFGFW1kTTdnO8sGnwPEzUvx2U2nyECwWmUJPRhLxbPPK+ep8eYMxlTxVO/wnQS5WmsGIKcrPP7/Fw1WVc=","balance":1099219665111,"nonce":0},{"type":"gov","id":"developer_reward","value":429,"lock":0},{"type":"account","pubkey":"BC80oG/EAXojuLCjIjQmIQgTv9wHscgCccEy4q7R2Vwak2iPbrTb1htgVOU+NjChxxmOeNiUJMxURPqUEWZ2lzc=","balance":1555171871,"nonce":26093}],
"following":[{"type":"gov","id":"block_reward","value":1420,"lock":0},{"type":"account","pubkey":"BL0SzhkFGFW1kTTdnO8sGnwPEzUvx2U2nyECwWmUJPRhLxbPPK+ep8eYMxlTxVO/wnQS5WmsGIKcrPP7/Fw1WVc=","balance":1099222921807,"nonce":0},{"type":"gov","id":"developer_reward","value":429,"lock":0},{"type":"account","pubkey":"BC80oG/EAXojuLCjIjQmIQgTv9wHscgCccEy4q7R2Vwak2iPbrTb1htgVOU+NjChxxmOeNiUJMxURPqUEWZ2lzc=","balance":1571463498,"nonce":26093}]}
```
