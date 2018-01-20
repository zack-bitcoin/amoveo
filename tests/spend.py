from get_request import request

def spend_test():
    print("spend test")
    pub = "BGRv3asifl1g/nACvsJoJiB1UiKU7Ll8O1jN/VD2l/rV95aRPrMm1cfV1917dxXVERzaaBGYtsGB5ET+4aYz7ws="
    priv = "nJgWyLTX1La8eCbPv85r3xs7DfmJ9AG4tLrJ5fiW6qY="
    brainwallet = ''
    request(2, "load_key", [pub, priv, brainwallet], 1)
    request(1, "create_account", [pub, 1], 0.1)
    request(1, "sync", [[127,0,0,1], 3020], 0.1)
    request(1, "spend", [pub, 2])
    request(1, "spend", [pub, 3])
    request(1, "sync", [[127,0,0,1], 3020], 0.1)
    request(1, "mine_block", [1,1], 0.3)
    request(1, "sync", [[127,0,0,1], 3020])

if __name__ == "__main__":
    spend_test()
