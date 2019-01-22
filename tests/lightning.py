from get_request import request

def assertEqual(x, y):
    if (x != y):
        print("fail\n")

def lightning_test():
    print("lightning test")
    request(2, "add_peer", [[127,0,0,1], 3010], 5)
    request(2, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(1, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(1, "add_peer", [[127,0,0,1], 3010])
    request(1, 'mine_block', [1, 10000000], 1)
    request(1, 'mine_block', [1, 10000000], 1)
    request(2, 'sync', [[127,0,0,1], 3010], 0.4)
    request(3, 'sync', [[127,0,0,1], 3010], 0.4)
#def dont_doit():
    pub2 = 'BEdcBeV8yXcki/s2Lk2aJoCG59/82yacIKdYSW+5p6ZahDZoIUnOiA790dj3KsNSwgdqq1L6IPU5bcq4+ukGCgI='
    priv2 = 'NQNPEkn+ERzNCH0T4FPYzv3PEXl36S5cGGP0NNMS/Fo='
    pub3 = 'BFRjuCgudSTRU79SVoCBvWi55+N1QethvQI6LKUCoEPHvIfedkQLxnuD2VJHqoLrULmXyexRWs2sOTwyLsdyL+E='
    priv3 = 'IxHs+qu1daOGQ/PfBN4LHM3h2W/5X3dGYfb4q3lkupw='
    brainwallet = ''
    request(1, 'dump_channels', [])
    request(2, 'dump_channels', [])
    request(3, 'dump_channels', [], 0.05)
    request(2, 'load_key', [pub2, priv2, brainwallet])
    request(3, 'load_key', [pub3, priv3, brainwallet])
    request(1, 'create_account', [pub2, 1000000000], 0.5)
#def dont_doit():
    request(1, 'create_account', [pub3, 1000000000], 10)
    request(1, 'txs', [[127,0,0,1], 3030], 0.5)
    request(1, 'txs', [[127,0,0,1], 3020], 0.5)
    #request(3, 'sync', [[127,0,0,1], 3010], 0.5)
    #request(2, 'sync', [[127,0,0,1], 3010], 0.5)
#def dont_doit():
    fee = 152000
    cid1 = 'vVhSBIjO7fU0V4v08WH2O2crgjtl9wTODuIk+jeB2NM='
    cid2 = '7zCJZIMatujoQjVXrPiTMMPkXOBiT/oOhY24q+mYAZo='
    request(1, 'new_channel_with_server', [[127,0,0,1], 3030, cid1, 10000, 9999, fee, 100, 1000], 0.05)
    request(2, 'sync', [[127,0,0,1], 3030], 1.8)
    request(2, 'new_channel_with_server', [[127,0,0,1], 3030, cid2, 10000, 9999, fee, 100, 1000], 0.4)
    request(1, 'sync', [[127,0,0,1], 3030], 0.2)
    request(1, 'txs', [[127,0,0,1], 3030], 0.5)
    request(1, 'txs', [[127,0,0,1], 3020], 2)
    request(1, 'channel_spend', [[127,0,0,1], 3030, 777], 0.05)
    request(1, 'lightning_spend', [[127,0,0,1], 3030, pub2, 4, 10], 0.05)
    request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.05)
    request(1, 'pull_channel_state', [[127,0,0,1], 3030], 0.05)
    pub1 = 'BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo='
    request(2, 'lightning_spend', [[127,0,0,1], 3030, pub1, 4000, 10], 0.05)
    request(1, 'pull_channel_state', [[127,0,0,1], 3030], 0.05)
    request(2, 'pull_channel_state', [[127,0,0,1], 3030], 0.05)
    request(1, 'sync', [[127,0,0,1], 3030], 0.05)
    request(1, 'sync', [[127,0,0,1], 3020], 0.3)
    height1 = request(1, 'height', [], 0.05)
    height2 = request(2, 'height', [], 0.05)
    height3 = request(3, 'height', [], 0.05)
    assertEqual(height1, height2)
    assertEqual(height1, height3)
#<<<<<<< HEAD
    request(1, 'mine_block', [1, 10000000], 1)
    request(1, 'sync', [[127,0,0,1], 3020], 0.05)
    request(1, 'sync', [[127,0,0,1], 3030], 1)
    request(2, 'sync', [[127,0,0,1], 3010], 0.05)
    request(3, 'sync', [[127,0,0,1], 3010], 1)
def dont_do():
    request(1, 'channel_close', [[127,0,0,1], 3030])
    request(2, 'channel_close', [[127,0,0,1], 3030])
    request(1, 'txs', [[127,0,0,1], 3020], 0.05)
    request(1, 'txs', [[127,0,0,1], 3030], 0.3)
    request(1, 'mine_block', [1, 10000000], 0.02)
    request(2, 'sync', [[127,0,0,1], 3010], 0.05)
    request(3, 'sync', [[127,0,0,1], 3010], 0.05)
#=======
    #request(1, 'channel_close', [[127,0,0,1], 3030], 0.1)
    #request(2, 'channel_close', [[127,0,0,1], 3030], 0.1)
    #request(1, 'txs', [[127,0,0,1], 3030], 0.05)
    #request(1, 'txs', [[127,0,0,1], 3020], 0.3)
    #request(1, 'mine_block', [1, 10000000], 0.05)
    #request(1, 'sync', [[127,0,0,1], 3030], 0.05)
    #request(1, 'sync', [[127,0,0,1], 3020], 0.3)
#>>>>>>> master

if __name__ == "__main__":
    lightning_test()
