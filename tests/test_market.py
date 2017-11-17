from base import ApiUser, DEV_1_INT, DEV_2_INT, DEV_3_INT
from nose.tools import nottest

#@nottest
class MarketTest(ApiUser):
    def test_market(self):
        self.request('mine_block', DEV_1_INT, [1, 1], sleep=0.4)
        #self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=2)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        pub1 = "BOLh/UTJK6g4bgC4hSh941OEVdNfTBvqAU5OvgWWL3Dnv8M/dy6oioTIH9fHXdWaXXPop1BxQ/x3MfoEd3lnV7g="
        priv1 = "JCltJID7JJxG8c6PJ2XBe4a+nIF9RIcWSxA0ws+XWZ4="
        pub2 = "BJDmrdYxlZiG3hTyzcqzBVHJIhX2fUYHH2K+Q2usFVIdPWnaOLdlTAUtAqQLQ6h/XR7qiAjGnLxfyCPIbXF+2hg="
        priv2 = "VpYenRK1E+pBMhfstAEZ65+UE/nPAoNd0uiNsxD7/w8="
        brainwallet = ""
        self.request("dump_channels", DEV_1_INT, [])
        self.request("dump_channels", DEV_2_INT, [])
        self.request("dump_channels", DEV_3_INT, [], sleep=0.04)
        self.load_key(DEV_2_INT, [pub1, priv1, brainwallet], sleep=0)
        self.load_key(DEV_3_INT, [pub2, priv2, brainwallet], sleep=0.04)
        self.create_account(DEV_1_INT, [pub1, 1000000000], sleep=0.04)
        self.create_account(DEV_1_INT, [pub2, 1000000000], sleep=0.1)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.04)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        self.request('new_channel_with_server', DEV_1_INT, [[127, 0, 0, 1], 3030, 17, 10000, 9999, 50, 4], sleep=0.04)
        self.sync(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.2)
        self.request('new_channel_with_server', DEV_2_INT, [[127, 0, 0, 1], 3030, 27, 10000, 9999, 50, 4], sleep=0.04)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.04)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.04)
        self.request('new_question_oracle', DEV_1_INT, [0, 'aXMgMisyPTQ/'], sleep=0.04)
        self.request('mine_block', DEV_1_INT, [10, 1], sleep=1)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.2)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.2)
        oid = 1
        self.request('new_market', DEV_3_INT, [oid, 20, 10], sleep=0.1)
        #self.request('trade', DEV_1_INT, [1000, 1, 1, oid, 20, [127,0,0,1], 3030], sleep=0.04)
        self.request('trade', DEV_1_INT, [3000, 1, 2, oid, 20, [127,0,0,1], 3030], sleep=0.04)
        self.request('trade', DEV_1_INT, [6000, 2, 2, oid, 20, [127,0,0,1], 3030], sleep=0.04)
        self.request('trade', DEV_1_INT, [8000, 2, 2, oid, 20, [127,0,0,1], 3030], sleep=0.04)
        self.request('trade', DEV_1_INT, [9000, 2, 1, oid, 20, [127,0,0,1], 3030], sleep=0.04)
    #def dont_doit(): #good for testing market gui overlaped open trades stuff.
        self.request('mine_block', DEV_1_INT, [11, 1], sleep=1)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.2)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.2)
        self.request('mine_block', DEV_1_INT, [11, 1], sleep=1)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.2)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.2)
        self.request('pull_channel_state', DEV_2_INT, [[127,0,0,1], 3030], sleep=0.04)
        self.request('mine_block', DEV_1_INT, [1, 1], sleep=0.1)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.5)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.5)
        #self.request('pull_channel_state', DEV_1_INT, [[127,0,0,1], 3030], sleep=0.04)
    #def dont_doit(): #good for testing market gui stuff.
        self.request('combine_cancel_assets', DEV_1_INT, [[127,0,0,1], 3030], sleep = 0.04)
        self.request('oracle_bet', DEV_1_INT, [oid, 1, 26900000000], sleep=0.04)
        self.request('mine_block', DEV_1_INT, [11, 1], sleep=1)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.2)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.2)
        self.request('oracle_close', DEV_1_INT, [oid], sleep=0.4)
        self.request('mine_block', DEV_1_INT, [1, 1], sleep=0.4)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.3)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=2)
        self.request('mine_block', DEV_1_INT, [1, 1], sleep=0.1)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.3)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.3)
        self.request('oracle_shares', DEV_1_INT, [oid], sleep=0.04)
        self.request('oracle_unmatched', DEV_1_INT, [oid], sleep=0.04)
        #self.request('pull_channel_state', DEV_1_INT, [[127,0,0,1], 3030], sleep=0.04)
        self.request('pull_channel_state', DEV_2_INT, [[127,0,0,1], 3030], sleep=0.04)
        self.request('mine_block', DEV_1_INT, [1, 1], sleep=0.2)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.04)
        self.request('sync', DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=1)
