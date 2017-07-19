from base import ApiUser, DEV_1_INT, DEV_2_INT
from nose.tools import nottest

#@nottest
class SpendTest(ApiUser):
    def test_mine_and_sync(self):
        pub = "BGRv3asifl1g/nACvsJoJiB1UiKU7Ll8O1jN/VD2l/rV95aRPrMm1cfV1917dxXVERzaaBGYtsGB5ET+4aYz7ws="
        priv = "nJgWyLTX1La8eCbPv85r3xs7DfmJ9AG4tLrJ5fiW6qY="
        brainwallet = ''
        self.load_key(DEV_2_INT, [pub, priv, brainwallet])
        self.add_peer(DEV_2_INT, [[127, 0, 0, 1], 3010])
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)

        self.create_account(DEV_1_INT, [pub, 1])
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)

        self.spend(DEV_1_INT, [pub, 5])
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)

        #self.mine_block(DEV_1_INT, [])
        self.mine_block(DEV_1_INT, [1, 1], sleep=0.3)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020])
