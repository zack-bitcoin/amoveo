from time import sleep
from swagger import SwaggerTest, IntAPI
from nose.tools import nottest


class InternalAPITest(SwaggerTest):
    API = {
        'dev1': IntAPI('localhost', 3012),
        'dev2': IntAPI('localhost', 3022),
        'dev3': IntAPI('localhost', 3032),
    }
    URL = {
        'dev1': 'http://localhost:3012/v1',
        'dev2': 'http://localhost:3022/v1',
        'dev3': 'http://localhost:3032/v1'
    }

    def setUp(self):
        api = self.API['dev1']
        # save master key pair
        self.master_pub, self.master_priv = self.c(api.fetch_keypair())
        self.assertIsNotNone(self.master_pub)
        self.assertIsNotNone(self.master_priv)

    def tearDown(self):
        api = self.API['dev1']
        # restore master key pair
        self.c(api.set_keypair(self.master_pub, self.master_priv))

    def test_create_keypair(self):
        api = self.API['dev1']
        pub, priv = self.c(api.create_keypair())
        self.assertIsNotNone(pub)
        self.assertIsNotNone(priv)

    def test_create_account(self):
        api = self.API['dev1']
        pub, priv = self.c(api.create_account(10))
        self.assertIsNotNone(pub)
        self.assertIsNotNone(priv)

    def test_top(self):
        api = self.API['dev1']
        _hash, height = self.c(api.top())
        self.assertIsNotNone(_hash)
        self.assertIsNotNone(height)

    def test_add_peer(self):
        api = self.API['dev1']
        self.c(api.add_peer('46.101.103.165', 8080))
        self.c(api.add_peer('46.101.103165', 8080), 405)

    def test_spend(self):
        api = self.API['dev1']
        pub, _ = self.c(api.create_account(10))
        self.c(api.spend(pub, 5))

    def test_sync(self):
        api = self.API['dev1']
        self.c(api.sync('127.0.0.1', 3020))
        self.c(api.sync('127.0.0.1.1', 3020), 405)

    def test_mine_block(self):
        api = self.API['dev1']
        self.c(api.mine_block(2, 1))

    def test_delete_account(self):
        api = self.API['dev1']
        master = self.c(api.fetch_pubkey())
        pub1, priv1 = self.c(api.create_account(20))
        self.c(api.set_keypair(pub1, priv1)); sleep(1)
        pub2, _ = self.c(api.create_account(10)); sleep(0.5)
        self.c(api.delete_account(pub2)); sleep(0.5)
        self.c(api.fetch_account(master))
        self.c(api.fetch_account(pub1), 404)
        self.c(api.fetch_account(pub2))
        # XXX check pub2's balance?

    # todo: fix api:integer_channel_balance/0
    @nottest
    def test_channel_balance(self):
        api = self.API['dev1']
        balance = self.c(api.channel_balance())
        self.assertIsNotNone(balance)

    @nottest
    def test_channel_solo_close(self):
        api = self.API['dev1']
        self.c(api.channel_solo_close())

    @nottest
    def test_lightning_payments(self):
        api1 = self.API['dev1']
        api2 = self.API['dev2']
        api3 = self.API['dev3']
        # grab dev1's pubkey
        pub1 = self.c(api1.fetch_pubkey())
        # sync dev1 with dev2 and dev3
        self.c(api1.sync('127.0.0.1', 3020)); sleep(0.5)
        self.c(api1.sync('127.0.0.1', 3030)); sleep(0.5)
        # set keys on dev2 and dev3
        pub2, priv2 = self.c(api2.create_keypair())
        self.c(api2.set_keypair(pub2, priv2))
        pub3, priv3 = self.c(api3.create_keypair())
        self.c(api3.set_keypair(pub3, priv3))
        # let dev1 know about dev2 and dev3
        self.c(api1.create_account_with_key(pub2, 10)); sleep(0.5)
        self.c(api1.create_account_with_key(pub3, 10)); sleep(0.5)
        # sync dev1 and dev2 with dev3
        self.c(api1.sync('127.0.0.1', 3030)); sleep(0.1)
        self.c(api2.sync('127.0.0.1', 3030)); sleep(0.1)
        # 2 step handshake to make channel
        self.c(api1.new_channel_with_server('127.0.0.1', 3030, 1, 10000, 10001, 50, 4)); sleep(0.1)
        self.c(api2.sync('127.0.0.1', 3030)); sleep(0.1)
        self.c(api2.new_channel_with_server('127.0.0.1', 3030, 2, 10000, 10001, 50, 4)); sleep(0.1)
        self.c(api1.sync('127.0.0.1', 3030)); sleep(0.1)
        # spend
        self.c(api1.channel_spend('127.0.0.1', 3030, 777)); sleep(0.1)
        self.c(api1.lightning_spend('127.0.0.1', 3030, pub2, 4, 10)); sleep(0.1)
        # XXX Can't pull dev1 first. Why?
        self.c(api2.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)
        self.c(api1.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)
        # spend again
        self.c(api2.lightning_spend('127.0.0.1', 3030, pub1, 4, 10)); sleep(0.1)
        self.c(api1.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)
        self.c(api2.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)

