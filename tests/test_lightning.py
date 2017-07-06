## FOR INVESTIGATION:
## THIS TESTS IS CONFLICTING WITH test_fork.py and tests.py (if you run any of these, test_lightning.py will fail)


# This quickly tests lightning payments. It is a lot faster and easier than using the blockchain to test the same thing.

# It lightning spends 4 tokens one way, then spends the same 4 back.

from base import ApiUser, DEV_1_INT, DEV_2_INT


class LightningTest(ApiUser):
    def test_payments(self):
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020])
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030])

        self.create_account(DEV_1_INT, ["OGlqQmhFUks0anBSVHp5Y1lDZGtVTjh0MWg5UDg2YTExMWs3N0RTUDZadUht", 10],
                            sleep=0.1)
        self.create_account(DEV_1_INT, ["MjFtZk5oeFFNWmphcVFzZzdVTHRZMTlQblN4b2dIQVRkSHg2SjRrSEF2MWdBag==", 10],
                            sleep=0.1)

        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)

        # 2 step handshake to make channel
        self.new_channel_with_server(DEV_1_INT, [[127, 0, 0, 1], 3030, 1, 10000, 10001, 50, 4], sleep=0.5)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.5)

        self.new_channel_with_server(DEV_2_INT, [[127, 0, 0, 1], 3030, 2, 10000, 10001, 50, 4])
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=1)

        self.channel_spend(DEV_1_INT, [[127, 0, 0, 1], 3030, 777], sleep=1)

        self.lightning_spend(DEV_1_INT, [[127, 0, 0, 1], 3030, 2,
                                         "BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=",
                                         4, 10], sleep=1)

        self.pull_channel_state(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=1)
        self.pull_channel_state(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=1)

        self.lightning_spend(DEV_2_INT, [[127, 0, 0, 1], 3030, 1,
                                         "BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=",
                                         4, 10], sleep=1)

        self.pull_channel_state(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=1)
        self.pull_channel_state(DEV_2_INT, [[127, 0, 0, 1], 3030])
