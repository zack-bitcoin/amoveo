from base import ApiUser, DEV_1_INT, DEV_2_INT, DEV_3_INT


class ForkTest(ApiUser):
    def test_mine_and_sync(self):
        self.mine_block(DEV_1_INT, [10, 1], sleep=1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010], sleep=1)

        self.mine_block(DEV_1_INT, [2, 1])
        self.mine_block(DEV_2_INT, [5, 1], sleep=1)

        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020])

    def test_three(self):
        self.mine_block(DEV_1_INT, [10, 1], sleep=1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3010])
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3010], sleep=1)

        self.mine_block(DEV_1_INT, [2, 1])
        self.mine_block(DEV_2_INT, [5, 1], sleep=1)

        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=1)
        self.sync(DEV_3_INT, [[127, 0, 0, 1], 3030])
