from get_request import request, request_ext
import json

def test1():
    request(1, "add_peer", [[127,0,0,1], 3020], 5)
    request(2, "add_peer", [[127,0,0,1], 3010])
    request(1, 'mine_block', [1, 10000000], 1)
    request(1, 'sync', [[127,0,0,1], 3020], 0.05)
    request(1, 'sync', [[127,0,0,1], 3030], 2)
    request(2, 'sync', [[127,0,0,1], 3010], 0.05)
    request(3, 'sync', [[127,0,0,1], 3010], 2)
    pub1 = "BGa4IH7VLKIeRXLijhN1ll/8Bje5EsmPNTaJu4VyXBCw1H+4SouUlAWhe6YrYpDRgWUdIOz56vEGunNqbzDG+7Q="
    priv1 = "djiLjyQ9/hRssa94acD3GCKl4aI5Axjbkk8Xytug7jU="
    pub2 = "BA5h8zRrS7f3mtR1DQPh4wk7FwvCBwZCD79QdTzTFvShgAdfQ3B7cNoWQhVS6Ubr290AfC0gkg2bGEBqNnllzJY="
    priv2 = "czZqvtgtctb8gXp0ziBONZsQrmKyx9XokbmYmYG2Djw="
    brainwallet = ""
    request(1, 'dump_channels', [])
    request(2, 'dump_channels', [])
    request(3, 'dump_channels', [], 0.05)
    request(2, 'load_key', [pub1, priv1, brainwallet])
    request(3, 'load_key', [pub2, priv2, brainwallet])
    request(1, 'create_account', [pub1, 1000000000], 0.04)
    request(1, 'create_account', [pub2, 1000000000], 0.4)
    height = json.loads(request(1, 'height', [], 0))[1]
    oid = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU="
    oid2 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY="
    request(1, 'txs', [[127,0,0,1], 3020], 2)
    request(2, 'txs', [[127,0,0,1], 3010], 2)
    #request(1, 'new_question_oracle', [height+1, 'aXMgMisyPTQ/', oid], 1)
    request(2, 'new_question_oracle', [height+1,'b3JhY2xlMg==', oid2], 1)
    request(1, 'txs', [[127,0,0,1], 3020], 2)
    request(2, 'txs', [[127,0,0,1], 3010], 2)
    request(1, 'mine_block', [27, 10000000], 1)
    request(1, 'sync', [[127,0,0,1], 3020], 1)
    request(2, 'sync', [[127,0,0,1], 3010], 2)
    #request(1, 'oracle_bet', [oid 2, 2650000000], 0.4)
    request(2, 'oracle_bet', [oid2, 2, 150000000], 0.4)#here
    request(1, 'sync', [[127,0,0,1], 3020], 1)
    request(2, 'sync', [[127,0,0,1], 3010], 2)
    request(1, 'mine_block', [11, 10000000], 2)
    request(1, 'sync', [[127,0,0,1], 3020], 1)
    request(2, 'sync', [[127,0,0,1], 3010], 2)
    #request(2, 'oracle_close', [oid], 0.4)
    request(2, 'delete_account', [pub2], 0.5)
    request(1, 'txs', [[127,0,0,1], 3020], 1)
    request(2, 'txs', [[127,0,0,1], 3010], 1)
    request(1, 'oracle_close', [oid2], 0.4)#here
    request(1, 'txs', [[127,0,0,1], 3020], 1)
    request(2, 'txs', [[127,0,0,1], 3010], 1)
    #request(1, 'oracle_winnings', [oid], 0.04)
    #request(1, 'oracle_unmatched', [oid], 0.04)
    #request(1, 'oracle_winnings', [oid2], 0.04)
    #request(1, 'oracle_unmatched', [oid2], 0.04)
    #request(1, 'txs', [[127,0,0,1], 3020], 2)
    #request(2, 'txs', [[127,0,0,1], 3010], 2)
    request(2, 'mine_block', [1, 10000], 0.2)
    request(1, 'sync', [[127,0,0,1], 3020], 0)
    request(2, 'sync', [[127,0,0,1], 3010], 0)
    
    return 0
    
if __name__ == "__main__":
    test1()
