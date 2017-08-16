from base import ApiUser, DEV_1_INT, DEV_2_INT, DEV_3_INT

class ForkTest(ApiUser):
    #@nottest
    def test_mine_and_sync(self):
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=0.1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=0.1)
        self.mine_block(DEV_1_INT, [1, 1], sleep=0.3)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=0.3)

        self.mine_block(DEV_1_INT, [1, 1], sleep=0.3)
        self.mine_block(DEV_2_INT, [2, 1], sleep=0.3)

        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.3)
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3010], sleep=0.3)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=0.1)
    #@nottest
    def test_three(self):
        self.mine_block(DEV_1_INT, [1, 1], sleep=0.3)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=0.3)
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3010], sleep=0.3)

        self.mine_block(DEV_2_INT, [1, 1], sleep=0.1)
        self.mine_block(DEV_1_INT, [2, 1], sleep=0.3)

        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.3)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
