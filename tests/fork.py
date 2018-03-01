from get_request import request

def assertEqual(x, y):
    if (x != y):
        print(x)
        print(y)
        throw("fail")

def test_mine_and_sync():
    print("fork test: mine and sync test")
    request(2, "add_peer", [[127,0,0,1], 3010])
    request(2, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(1, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(1, "add_peer", [[127,0,0,1], 3010])
    request(2, 'sync', [[127,0,0,1], 3010], 0.1)
    request(1, 'sync', [[127,0,0,1], 3020], 0.1)
    request(1, 'mine_block', [2, 100000], 1)
    request(2, 'sync', [[127,0,0,1], 3010], 1)
    request(2, 'mine_block', [2, 100000], 0)
    request(1, 'mine_block', [1, 100000], 2)
    request(2, 'sync', [[127,0,0,1], 3010], 0.5)
    request(3, 'sync', [[127,0,0,1], 3010], 0.3)
    height1 = request(1, 'height', [], 0.05)
    height2 = request(2, 'height', [], 0.05)
    height3 = request(3, 'height', [], 0.05)
    assertEqual(height1, height2)
    assertEqual(height1, height3)
    #assertEqual(height1, "[\"ok\",4]")
    
def test_three():
    print("fork test: sync three nodes test")
    request(1, 'mine_block', [1, 1000000], 1.5)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    request(3, 'sync', [[127,0,0,1], 3010], 0.5)
    request(2, 'mine_block', [1, 1000000], 0)
    request(1, 'mine_block', [2, 1000000], 2)
    request(3, 'sync', [[127,0,0,1], 3010], 0.3)
    request(3, 'sync', [[127,0,0,1], 3020], 0.1)
    height1 = request(1, 'height', [], 0.05)
    height2 = request(2, 'height', [], 0.05)
    assertEqual(height1, height2)


if __name__ == "__main__":
    test_mine_and_sync()
    test_three()
