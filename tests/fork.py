from get_request import request

def test_mine_and_sync():
    print("fork test: mine and sync test")
    request(2, 'sync', [[127,0,0,1], 3010], 0.1)
    request(1, 'mine_block', [2, 100000], 0.3)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    request(1, 'mine_block', [1, 100000], 0.3)
    request(2, 'mine_block', [2, 100000], 0.3)
    request(1, 'sync', [[127,0,0,1], 3020], 0.3)
    request(3, 'sync', [[127,0,0,1], 3010], 0.3)
    request(2, 'sync', [[127,0,0,1], 3010], 0.1)
    
def test_three():
    print("fork test: sync three nodes test")
    request(1, 'mine_block', [1, 1000000], 0.3)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    request(3, 'sync', [[127,0,0,1], 3010], 0.5)
    request(2, 'mine_block', [1, 1000000], 0.1)
    request(1, 'mine_block', [2, 1000000], 0.3)
    request(1, 'sync', [[127,0,0,1], 3030], 0.3)
    request(2, 'sync', [[127,0,0,1], 3030], 0.1)


if __name__ == "__main__":
    test_mine_and_sync()
    test_three()
