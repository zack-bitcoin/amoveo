from base import ApiUser, DEV_1_INT, DEV_2_INT


class ShareBlocksTest(ApiUser):
    def test(self):
        self.add_peer(DEV_2_INT, [[127, 0, 0, 1], 3010])
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3020])

        self.mine_block(DEV_1_INT, [1, 1], sleep=0.2)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.2)

        self.mine_block(DEV_2_INT, [1, 1], sleep=0.2)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep =0.2)

        self.mine_block(DEV_1_INT, [1, 1], sleep=0.2)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep =0.2)
