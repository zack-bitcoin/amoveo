
from get_request import request

def assertEqual(x, y):
    if (x != y):
        print(x)
        print(y)
        throw("fail")

def test_mine_and_sync():
    print("fork test: mine and sync test")
    request(1, 'mine_block', [1, 100000])
    request(2, 'mine_block', [1, 100000])
    request(3, 'mine_block', [1, 100000], 1)
    request(2, "add_peer", [[127,0,0,1], 3010])
    request(2, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3030])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(3, "add_peer", [[127,0,0,1], 3010])
    request(3, "add_peer", [[127,0,0,1], 3020])
    request(2, 'sync', [[127,0,0,1], 3010], 0.1)
    request(1, 'sync', [[127,0,0,1], 3020], 0.1)
    request(1, 'mine_block', [2, 100000], 1)
    request(2, 'sync', [[127,0,0,1], 3010], 1)
    request(2, 'mine_block', [12, 100000], 0)
    request(1, 'mine_block', [11, 100000], 10)
#    request(2, 'sync', [[127,0,0,1], 3010], 0.5)
#    request(1, 'sync', [[127,0,0,1], 3030], 0.3)
#    request(3, 'sync', [[127,0,0,1], 3010], 0.3)
#    request(3, 'sync', [[127,0,0,1], 3010], 10)
    height1 = request(1, 'height', [1], 0.05)
    height2 = request(2, 'height', [1], 0.05)
    height3 = request(3, 'height', [1], 0.05)
    assertEqual(height1, height2)
    assertEqual(height1, height3)
    #assertEqual(1, 2)
    #assertEqual(height1, "[\"ok\",4]")
    
def test_three():
    print("fork test: sync three nodes test")
    request(1, 'mine_block', [1, 1000000], 1.5)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    request(3, 'sync', [[127,0,0,1], 3010], 0.5)
    request(2, 'mine_block', [1, 1000000], 0)
    request(1, 'mine_block', [2, 1000000], 2)
    request(3, 'sync', [[127,0,0,1], 3010], 0.3)
    request(3, 'sync', [[127,0,0,1], 3020], 0.3)
    height1 = request(1, 'height', [], 0.05)
    height2 = request(2, 'height', [], 0.05)
    assertEqual(height1, height2)

def test_orphan_txs():
    #pub3 = "BGRv3asifl1g/nACvsJoJiB1UiKU7Ll8O1jN/VD2l/rV95aRPrMm1cfV1917dxXVERzaaBGYtsGB5ET+4aYz7ws="
    pub2 = "BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8="
    print("orphan tx 0")
    request(2, "add_peer", [[127,0,0,1], 3010])
    request(1, "add_peer", [[127,0,0,1], 3020])
    request(2, 'mine_block', [1, 1000000], 0.1)
    request(1, 'mine_block', [10, 1000000], 2.5)
    request(1, "spend", [pub2, 1000], 0.1)
    request(1, 'mine_block', [2, 1000000], 0.1)
    request(2, 'sync', [2, [127,0,0,1], 3010], 0.2)
    request(2, 'sync', [[127,0,0,1], 3010], 0.2)
    #request(1, 'sync', [[127,0,0,1], 3020], 2)
    request(2, 'mine_block', [2, 1000000], 0.4)
    height1 = request(1, 'height', [1], 0.05)
    height2 = request(2, 'height', [1], 0.05)
    print(height1)
    print(height2)
    request(1, 'sync', [2, [127,0,0,1], 3020], 0.2)
    request(1, 'sync', [[127,0,0,1], 3020], 0.2)
    request(2, 'sync', [2, [127,0,0,1], 3010], 2)
    request(2, 'sync', [[127,0,0,1], 3010], 2)
    print("orphan tx 1")
    height1 = request(1, 'height', [1], 0.05)
    height2 = request(2, 'height', [1], 0.05)
    assertEqual(height1, height2)
    print("orphan tx 1.5")
    print(height1)#10
    print(height2)#10

    #remove peers
    request(1, 'add_peer', [0, 0], 0.5)
    request(2, 'add_peer', [0, 0], 0.5)

    print("orphan tx 2")

    request(1, "spend", [pub2, 1], 1)
    request(1, "spend", [pub2, 1], 1)
    #request(1, 'txs', [[127,0,0,1], 3020], 1)

    #assertEqual(1, 2)#lets see what server2's view of trees:get(accounts, pub2) is

    request(2, "spend", [pub2, 10], 0.1)
    request(2, "spend", [pub2, 10], 0.1)
    request(2, "spend", [pub2, 10], 0.1)
    #request(2, "spend", [pub2, 1], 1)

    print("orphan tx 2.2")
    request(2, 'mine_block', [], 0.1)
    request(1, 'mine_block', [], 0.5)
    assertEqual(1, 2)
    request(1, 'mine_block', [], 0.1)
    height1 = request(1, 'height', [1], 0.05)
    height2 = request(2, 'height', [1], 0.05)
    #assertEqual(1, 2)
    #assertEqual(height1, height2)
    print("orphan tx 2.5")
    print(height1)#10
    print(height2)#10
    #assertEqual(1, 2)

    #THIS IS WHERE THE TX GETS DROPPED.
    print("orphan tx 3")
    request(2, 'sync', [2, [127,0,0,1], 3010], 0.5)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    request(1, 'sync', [[127,0,0,1], 3020], 0.5)
    #print("orphan tx 4")
    height1 = request(1, 'height', [1], 0.05)
    height2 = request(2, 'height', [1], 0.05)
    #print(height1)#11
    assertEqual(height1, height2)
    request(1, 'mine_block', [1, 1000000], 0.5)
    request(2, 'sync', [2, [127,0,0,1], 3010], 0.5)
    request(2, 'sync', [[127,0,0,1], 3010], 0.3)
    #request(1, 'sync', [[127,0,0,1], 3020], 0.5)
    #print("orphan tx 6")
    #request(2, "add_peer", [[127,0,0,1], 3010])
    #request(1, "add_peer", [[127,0,0,1], 3020])
    #print("orphan tx 7")
    height1 = request(1, 'height', [1], 0.05)
    height2 = request(2, 'height', [1], 0.05)
    assertEqual(height1, height2)
    print("orphan tx 8")
    a = request(1, "account", [pub2], 0.5)
    print(a)
    print("orphan tx test finished")
    assertEqual(1, 2)

    

if __name__ == "__main__":
    test_mine_and_sync()
    test_three()
    test_orphan_txs()
