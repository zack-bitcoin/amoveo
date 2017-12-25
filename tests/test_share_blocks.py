from base import ApiUser, DEV_1_INT, DEV_2_INT
from nose.tools import nottest

#@nottest
class ShareBlocksTest(ApiUser):
    def test(self):
        self.add_peer(DEV_2_INT, [[127, 0, 0, 1], 3010])
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3020])

        self.mine_block(DEV_1_INT, [1, 100000], sleep=0.2)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=2.0)

        self.mine_block(DEV_2_INT, [1, 100000], sleep=0.2)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep =0.2)

        self.mine_block(DEV_1_INT, [100, 100000], sleep=0.2)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep =0.2)
