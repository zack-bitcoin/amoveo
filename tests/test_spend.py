from base import ApiUser, DEV_1_INT, DEV_2_INT


class SpendTest(ApiUser):
    def test_mine_and_sync(self):
        self.add_peer(DEV_2_INT, [[127, 0, 0, 1], 3010])
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=1)

        self.create_account(DEV_1_INT, ["BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=", 1])
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=1)

        self.spend(DEV_1_INT, [27, 100])
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=1)

        self.mine_block(DEV_1_INT, [])
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020])
