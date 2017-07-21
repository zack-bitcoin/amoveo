## FOR INVESTIGATION:
## THIS TESTS IS CONFLICTING WITH test_fork.py and tests.py (if you run any of these, test_lightning.py will fail)


# This quickly tests lightning payments. It is a lot faster and easier than using the blockchain to test the same thing.

# It lightning spends 4 tokens one way, then spends the same 4 back.

from base import ApiUser, DEV_1_INT, DEV_2_INT, DEV_3_INT


class LightningTest(ApiUser):
    def test_payments(self):
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.5)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.5)

        #self.create_account(DEV_1_INT, ["BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=", 10],
        #sleep=0.1)
        pub1 = 'BEdcBeV8yXcki/s2Lk2aJoCG59/82yacIKdYSW+5p6ZahDZoIUnOiA790dj3KsNSwgdqq1L6IPU5bcq4+ukGCgI='
        priv1 = 'NQNPEkn+ERzNCH0T4FPYzv3PEXl36S5cGGP0NNMS/Fo='
        pub2 = 'BFRjuCgudSTRU79SVoCBvWi55+N1QethvQI6LKUCoEPHvIfedkQLxnuD2VJHqoLrULmXyexRWs2sOTwyLsdyL+E='
        priv2 = 'IxHs+qu1daOGQ/PfBN4LHM3h2W/5X3dGYfb4q3lkupw='
        brainwallet = ''
        self.load_key(DEV_2_INT, [pub1, priv1, brainwallet])
        self.load_key(DEV_3_INT, [pub2, priv2, brainwallet], sleep=0.5)
        self.create_account(DEV_1_INT, [pub1, 10], sleep=0.5)
        self.create_account(DEV_1_INT, [pub2, 10], sleep=0.5)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)

        #self.create_account(DEV_1_INT, ["BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=", 10],
        #sleep=0.1)
        pub1 = "BEdcBeV8yXcki/s2Lk2aJoCG59/82yacIKdYSW+5p6ZahDZoIUnOiA790dj3KsNSwgdqq1L6IPU5bcq4+ukGCgI="
        priv1 = 'NQNPEkn+ERzNCH0T4FPYzv3PEXl36S5cGGP0NNMS/Fo='
        pub2 = 'BFRjuCgudSTRU79SVoCBvWi55+N1QethvQI6LKUCoEPHvIfedkQLxnuD2VJHqoLrULmXyexRWs2sOTwyLsdyL+E='
        priv2 = 'IxHs+qu1daOGQ/PfBN4LHM3h2W/5X3dGYfb4q3lkupw='
        brainwallet = ''
        self.request("dump_channels", DEV_1_INT, [], sleep=0.1)
        self.request("dump_channels", DEV_2_INT, [], sleep=0.1)
        self.request("dump_channels", DEV_3_INT, [], sleep=0.1)
        self.load_key(DEV_2_INT, [pub1, priv1, brainwallet], sleep=1)
        self.load_key(DEV_3_INT, [pub2, priv2, brainwallet], sleep=1)
        self.create_account(DEV_1_INT, [pub1, 10], sleep=0.1)
        self.create_account(DEV_1_INT, [pub2, 10], sleep=0.1)

        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        #self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)

        # 2 step handshake to make channel
        self.new_channel_with_server(DEV_1_INT, [[127, 0, 0, 1], 3030, 1, 10000, 10001, 50, 4], sleep=0.1)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.new_channel_with_server(DEV_2_INT, [[127, 0, 0, 1], 3030, 2, 10000, 10001, 50, 4], sleep=0.1)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)

        self.channel_spend(DEV_1_INT, [[127, 0, 0, 1], 3030, 777], sleep=0.1)

        self.lightning_spend(DEV_1_INT, [[127, 0, 0, 1], 3030, pub1, 4, 10], sleep=0.1)
        self.pull_channel_state(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.pull_channel_state(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)

        pub3 = 'BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo='
        self.lightning_spend(DEV_2_INT, [[127, 0, 0, 1], 3030, pub3, 4, 10], sleep=1)
        self.pull_channel_state(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.pull_channel_state(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)

