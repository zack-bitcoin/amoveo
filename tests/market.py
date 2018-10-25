from get_request import request
import json

def market_test():
    print("market test")
    request(2, "add_peer", [[127,0,0,1], 3010], 5)
    request(2, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(1, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(1, "add_peer", [[127,0,0,1], 3010])
    request(1, 'mine_block', [1, 10000000], 1)
    request(1, 'sync', [[127,0,0,1], 3020], 0.05)
    request(1, 'sync', [[127,0,0,1], 3030], 2)
    request(2, 'sync', [[127,0,0,1], 3010], 0.05)
    request(3, 'sync', [[127,0,0,1], 3010], 2)
    pub1 = "BOLh/UTJK6g4bgC4hSh941OEVdNfTBvqAU5OvgWWL3Dnv8M/dy6oioTIH9fHXdWaXXPop1BxQ/x3MfoEd3lnV7g="
    priv1 = "JCltJID7JJxG8c6PJ2XBe4a+nIF9RIcWSxA0ws+XWZ4="
    pub2 = "BJDmrdYxlZiG3hTyzcqzBVHJIhX2fUYHH2K+Q2usFVIdPWnaOLdlTAUtAqQLQ6h/XR7qiAjGnLxfyCPIbXF+2hg="
    priv2 = "VpYenRK1E+pBMhfstAEZ65+UE/nPAoNd0uiNsxD7/w8="
    brainwallet = ""
    request(1, 'dump_channels', [])
    request(2, 'dump_channels', [])
    request(3, 'dump_channels', [], 0.05)
    request(2, 'load_key', [pub1, priv1, brainwallet])
    request(3, 'load_key', [pub2, priv2, brainwallet])
    request(1, 'create_account', [pub1, 1000000000], 0.04)
    request(1, 'create_account', [pub2, 1000000000], 0.4)
    pub_light = "BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4="
    request(1, 'create_account', [pub_light, 1000000000], 2)
    request(1, 'txs', [[127,0,0,1], 3030], 0)
    #request(2, 'sync', [[127,0,0,1], 3030], 10)
    request(1, 'txs', [[127,0,0,1], 3020], 1)
#def dont_doit():
    fee = 152000
    cid1 = 'vVhSBIjO7fU0V4v08WH2O2crgjtl9wTODuIk+jeB2NM='
    cid2 = '7zCJZIMatujoQjVXrPiTMMPkXOBiT/oOhY24q+mYAZo='
    request(1, 'new_channel_with_server', [[127,0,0,1], 3030, cid1, 10000, 9999, fee, 100, 1000], 0.1)
    request(2, 'sync', [[127,0,0,1], 3030], 0)
    request(2, 'sync', [[127,0,0,1], 3010], 2)
    request(2, 'new_channel_with_server', [[127,0,0,1], 3030, cid2, 10000, 9999, fee, 99, 1000], 0.1)
    request(1, 'sync', [[127,0,0,1], 3030], 0.04)
    request(1, 'sync', [[127,0,0,1], 3020], 0.2)
    oid = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI="
    height = json.loads(request(1, 'height', [], 0))[1]
    x = request(1, 'new_question_oracle', [height+1, 'aXMgMisyPTQ/', oid], 1)
    #oid = json.loads(x)[1]
    print("python oid is ")
    print(oid)
    request(1, 'txs', [[127,0,0,1], 3020], 2)
    request(1, 'mine_block', [4, 10000000], 3)
    request(1, 'sync', [[127,0,0,1], 3030], 2)
    request(3, 'sync', [[127,0,0,1], 3010], 1)
    request(3, 'sync', [[127,0,0,1], 3010], 1)
    request(3, 'new_market', [oid, height+19, 5], 2)
def test2(): #useful for testing market from light node.
    request(3, 'txs', [[127,0,0,1], 3020], 1)
    request(3, 'txs', [[127,0,0,1], 3010], 9)
    request(3, 'txs', [[127,0,0,1], 3020])
    oid = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI="
    height = request(3, 'height', [], 0.01)
    height = json.loads(height)[1]
    #request(1, 'trade', [9000, 1, 10, oid, height, 20, [127,0,0,1], 3030], 0.2)
    request(1, 'trade', [1000, 1, 250, oid, height, 20, [127,0,0,1], 3030], 0.2)
    request(1, 'trade', [6000, 1, 1000, oid, height, 20, [127,0,0,1], 3030], 0.2)
    request(1, 'trade', [6001, 1, 1000, oid, height, 20, [127,0,0,1], 3030], 0.2)#remove this when testing betting in light node.
    request(2, 'trade', [6000, 2, 1000, oid, height, 20, [127,0,0,1], 3030], 0.2)
    request(2, 'trade', [6001, 2, 1000, oid, height, 20, [127,0,0,1], 3030], 0.2)
    request(1, 'mine_block', [11, 1000000], 0.1)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)
    request(1, 'mine_block', [11, 1000000], 0.04)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)
    request(1, 'mine_block', [1, 10000], 0.04)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)
    #request(1, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)#this line should be removed
def test3(): #useful for testing market from light node.
    oid = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI="
    #request(1, 'combine_cancel_assets', [[127,0,0,1], 3030], 0.1)
    request(2, 'oracle_bet', [oid, 1, 50000000], 0.2)
    request(1, 'oracle_bet', [oid, 2, 2600000000], 0.4)
    request(1, 'mine_block', [11, 10000], 1)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.2)
    request(1, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)#this line should be optional
    #request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)#this line should be optional
    request(1, 'oracle_close', [oid], 0.4)
    request(1, 'mine_block', [1, 10000], 0.4)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 1)
    request(1, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)
    request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)
    request(1, 'mine_block', [1, 10000], 0.1)
    request(1, 'sync', [[127,0,0,1], 3030], 0.3)
    request(1, 'sync', [[127,0,0,1], 3020], 0.8)
    #request(2, 'oracle_winnings', [oid], 0.04)#this causes a crash, as it should.
    request(1, 'oracle_winnings', [oid], 0.04)
    request(1, 'oracle_unmatched', [oid], 0.04)
    request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.04)
    request(1, 'mine_block', [1, 10000], 0.2)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)

if __name__ == "__main__":
    market_test()
    test2()
    test3()
    
