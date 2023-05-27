from get_request import request

def checkpoint_test1():
    #in the config file, set the checkpoint_depth to 1 for this to work.
    #request(2, "add_peer", [[127,0,0,1], 3010], 1)
    #request(1, "add_peer", [[127,0,0,1], 3020], 1)
    request(1, "mine_block", [40,1000000], 1)
    request(2, "sync", [2, [127,0,0,1], 3010], 1)
    fail()
    #request(2, "sync", [3, [127,0,0,1], 3010], 5)
    #height3 = request(1, 'height', [], 0.05)
    #height4 = request(2, 'height', [], 0.05)
    #assertEqual(height3, height4)


if __name__ == "__main__":
    checkpoint_test1()
