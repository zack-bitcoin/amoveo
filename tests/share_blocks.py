from get_request import request

def share_blocks_test1():
    print("share blocks test 1")
    request(2, "add_peer", [[127,0,0,1], 3010])
    request(1, "add_peer", [[127,0,0,1], 3020], 0.01)
    request(1, "mine_block", [1, 100000], 0.02)
    request(1, "sync", [[127,0,0,1], 3020], 0.05)
    request(2, "mine_block", [1, 100000], 0.02)
    request(1, "sync", [[127,0,0,1], 3020], 0.05)
    #we should check that the heights are the same.
def share_blocks_test2():
    print("share blocks test 2")
    request(1, "mine_block", [50, 100000], 0.1)
    request(1, "sync", [[127,0,0,1], 3020], 0.1)
    request(1, "mine_block", [3, 100000], 0.02)
    request(1, "sync", [[127,0,0,1], 3020], 0.1)
    request(1, "mine_block", [3, 100000], 0.02)
    request(2, "sync", [[127,0,0,1], 3010], 0.1)
    request(3, "sync", [[127,0,0,1], 3010], 0.02)
    request(1, "spend", ["BHpLwieFVdD5F/z1mdScC9noIZ39HgnwvK8jHqRSBxjzWBssIR1X9LGr8QxTi8fUQws1Q5CGnmTk5dZwzdrGBi4=", 1000000000], 0.05)
    request(1, "spend", ["BOzTnfxKrkDkVl88BsLMl1E7gAbKK+83pHCt0ZzNEvyZQPKlL/n8lYLCXgrL4Mmi/6m2bzj+fejX8D52w4U9LkI=", 1000000000], 0.05)
    request(1, "spend", ["BLgYECLeI0Iq7SZqPqhoZocy3zF3ht+fPdYkjJh3OnPU1tr7+BpDbtXGNyzDF8w4gUzV7UvM4KelK6IIvQNZZ6w=", 1000000000], 0.05)
    request(1, "mine_block", [1, 100000], 0.02)
    request(3, "sync", [[127,0,0,1], 3010], 0.02)
    request(3, "mine_block", [10, 100000], 0.02)
    request(3, "sync", [[127,0,0,1], 3030], 0.1)

if __name__ == "__main__":
    share_blocks_test1()
    share_blocks_test2()
