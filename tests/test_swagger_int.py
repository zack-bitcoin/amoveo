from time import sleep
from swagger import SwaggerTest, IntAPI

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

