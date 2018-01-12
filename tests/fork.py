from get_request import request

def test_mine_and_sync():
    request(2, 'sync', [[127,0,0,1], 3010], 0.1)
    request(1, 'mine_block', [1, 1], 0.3)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    request(1, 'mine_block', [1, 1], 0.3)
    request(2, 'mine_block', [2, 1], 0.3)
    request(1, 'sync', [[127,0,0,1], 3020], 0.3)
    request(3, 'sync', [[127,0,0,1], 3010], 0.3)
    request(2, 'sync', [[127,0,0,1], 3010], 0.1)
    
def test_three():
    request(1, 'mine_block', [1, 1], 0.3)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    request(3, 'sync', [[127,0,0,1], 3010], 0.5)
    request(2, 'mine_block', [1, 1], 0.1)
    request(1, 'mine_block', [2, 1], 0.3)
    request(1, 'sync', [[127,0,0,1], 3030], 0.3)
    request(2, 'sync', [[127,0,0,1], 3030], 0.1)
