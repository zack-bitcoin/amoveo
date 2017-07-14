from base import ApiUser, DEV_1_INT, DEV_2_INT, DEV_3_INT
from nose.tools import nottest

#@nottest
class ForkTest(ApiUser):
    def test_mine_and_sync(self):
        self.mine_block(DEV_1_INT, [1, 1], sleep=0.1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=0.1)

        self.mine_block(DEV_1_INT, [1, 1], sleep=0.1)
        self.mine_block(DEV_2_INT, [2, 1], sleep=0.1)

        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)

    def test_three(self):
        self.mine_block(DEV_1_INT, [1, 1], sleep=0.1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=0.1)
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3010], sleep=0.1)

        self.mine_block(DEV_1_INT, [1, 1], sleep=0.1)
        self.mine_block(DEV_2_INT, [2, 1], sleep=0.1)

        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3010], sleep=0.1)
