from base import ApiUser, DEV_1_INT, DEV_2_INT, DEV_3_INT
from nose.tools import nottest

#@nottest
class ShareBlocksTest(ApiUser):
    def test(self):
        self.add_peer(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep = 0)
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep = 0.01)

        self.mine_block(DEV_1_INT, [1, 100000], sleep=0.02)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.05)

        self.mine_block(DEV_2_INT, [1, 100000], sleep=0.02)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep =0.05)
    def test_2(self):
        self.mine_block(DEV_1_INT, [50, 100000], sleep=0.1)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep =0.1)
        self.mine_block(DEV_1_INT, [3, 100000], sleep=0.02)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep =0.1)
        self.mine_block(DEV_1_INT, [3, 100000], sleep=0.02)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep =0.02)
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3010], sleep =0.02)
        self.request("spend", DEV_1_INT, ["BHpLwieFVdD5F/z1mdScC9noIZ39HgnwvK8jHqRSBxjzWBssIR1X9LGr8QxTi8fUQws1Q5CGnmTk5dZwzdrGBi4=", 1000000000], sleep = 0.05)
        self.request("spend", DEV_1_INT, ["BOzTnfxKrkDkVl88BsLMl1E7gAbKK+83pHCt0ZzNEvyZQPKlL/n8lYLCXgrL4Mmi/6m2bzj+fejX8D52w4U9LkI=", 1000000000], sleep = 0.05)
        self.request("spend", DEV_1_INT, ["BLgYECLeI0Iq7SZqPqhoZocy3zF3ht+fPdYkjJh3OnPU1tr7+BpDbtXGNyzDF8w4gUzV7UvM4KelK6IIvQNZZ6w=", 1000000000], sleep = 0.05)
        self.mine_block(DEV_1_INT, [1, 100000], sleep=0.02)
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3010], sleep=0.1)
        self.mine_block(DEV_3_INT, [10, 100000], sleep=0.02)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep =0.1)
        #Puts money into 1 and 3 because this is a useful situation for testing channels.
