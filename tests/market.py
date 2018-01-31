from get_request import request
import json

def market_test():
    print("market test")
    request(1, 'mine_block', [1, 10000000], 0.02)
    request(1, 'sync', [[127,0,0,1], 3020])
    request(1, 'sync', [[127,0,0,1], 3030], 0.01)
    pub1 = "BOLh/UTJK6g4bgC4hSh941OEVdNfTBvqAU5OvgWWL3Dnv8M/dy6oioTIH9fHXdWaXXPop1BxQ/x3MfoEd3lnV7g="
    priv1 = "JCltJID7JJxG8c6PJ2XBe4a+nIF9RIcWSxA0ws+XWZ4="
    pub2 = "BJDmrdYxlZiG3hTyzcqzBVHJIhX2fUYHH2K+Q2usFVIdPWnaOLdlTAUtAqQLQ6h/XR7qiAjGnLxfyCPIbXF+2hg="
    priv2 = "VpYenRK1E+pBMhfstAEZ65+UE/nPAoNd0uiNsxD7/w8="
    brainwallet = ""
    request(1, 'dump_channels', [])
    request(2, 'dump_channels', [])
    request(3, 'dump_channels', [], 0.04)
    request(2, 'load_key', [pub1, priv1, brainwallet])
    request(3, 'load_key', [pub2, priv2, brainwallet])
    request(1, 'create_account', [pub1, 1000000000], 0.04)
    request(1, 'create_account', [pub2, 1000000000], 0.1)
    pub_light = "BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4="
    request(1, 'create_account', [pub_light, 1000000000], 0.1)
    request(1, 'sync', [[127,0,0,1], 3030], 0.04)
    request(2, 'sync', [[127,0,0,1], 3030], 0.2)
    fee = 152000
    cid1 = 'vVhSBIjO7fU0V4v08WH2O2crgjtl9wTODuIk+jeB2NM='
    cid2 = '7zCJZIMatujoQjVXrPiTMMPkXOBiT/oOhY24q+mYAZo='
    request(1, 'new_channel_with_server', [[127,0,0,1], 3030, cid1, 10000, 9999, fee, 4, 1000], 0.04)
    request(2, 'sync', [[127,0,0,1], 3030], 0.2)
    request(2, 'new_channel_with_server', [[127,0,0,1], 3030, cid2, 10000, 9999, fee, 4, 1000], 0.04)
    request(1, 'sync', [[127,0,0,1], 3030], 0.04)
    request(1, 'sync', [[127,0,0,1], 3020], 0.2)
    oid = request(1, 'new_question_oracle', [0, 'aXMgMisyPTQ/'], 0.04)
    oid = json.loads(oid)[1]
    print("python oid is ")
    print(oid)
    request(1, 'mine_block', [1, 1000000], 0.1)
    request(1, 'sync', [[127,0,0,1], 3020])
    request(1, 'sync', [[127,0,0,1], 3030], 0.2)
    #oid = 1
    request(3, 'new_market', [oid, 20, 5], 0.2)
#def dont_doit(): #useful for testing market from light node.
    height = request(3, 'height', [], 0.01)
    height = json.loads(height)[1]
    request(1, 'trade', [6000, 1, 1000, oid, height, 20, [127,0,0,1], 3030], 0.2)
    request(1, 'trade', [6001, 1, 1000, oid, height, 20, [127,0,0,1], 3030], 0.04)
    request(2, 'trade', [6000, 2, 1000, oid, height, 20, [127,0,0,1], 3030], 0.04)
    request(2, 'trade', [6001, 2, 1000, oid, height, 20, [127,0,0,1], 3030], 0.04)
    request(1, 'mine_block', [11, 1000000], 0.1)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)
    request(1, 'mine_block', [11, 1000000], 0.04)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)
    request(1, 'mine_block', [1, 10000], 0.04)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)
    request(1, 'combine_cancel_assets', [[127,0,0,1], 3030], 0.1)
    request(1, 'oracle_bet', [oid, 1, 2600000000], 0.05)
    request(1, 'mine_block', [11, 10000], 1)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.2)
    request(1, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)
    request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)
    request(1, 'oracle_close', [oid], 0.4)
    request(1, 'mine_block', [1, 10000], 0.4)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.4)
    request(1, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)
    request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.2)
    request(1, 'mine_block', [1, 10000], 0.1)
    request(1, 'sync', [[127,0,0,1], 3030], 0.3)
    request(1, 'sync', [[127,0,0,1], 3020], 0.3)
    request(1, 'oracle_winnings', [oid], 0.04)
    request(1, 'oracle_unmatched', [oid], 0.04)
    request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.04)
    request(1, 'mine_block', [1, 10000], 0.2)
    request(1, 'sync', [[127,0,0,1], 3030])
    request(1, 'sync', [[127,0,0,1], 3020], 0.04)

if __name__ == "__main__":
    market_test()
