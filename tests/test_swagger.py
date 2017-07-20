from time import sleep
from base import ApiUser


class SwaggerTest(ApiUser):
    INT_URL = "http://localhost:3012/v1"
    EXT_URL = "http://localhost:3013/v1"

    def setUp(self):
        self.master_pub, self.master_priv = self._fetch_keypair()

    def tearDown(self):
        self._set_keypair(self.master_pub, self.master_priv)

    #
    # Utility functions
    #

    def _create_keypair(self):
        uri = self.INT_URL + "/create-keypair"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        o = response.json()
        return o['public'], o['private']

    def _fetch_keypair(self, status_code = 200):
        uri = self.INT_URL + "/fetch-keypair"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, status_code)
        o = response.json()
        return o['public'], o['private']

    def _set_keypair(self, public, private, brainwallet = ''):
        uri = self.INT_URL + "/set-keypair"
        data = {
            "public": public,
            "private": private,
            "brain-wallet": brainwallet
        }
        self.session.post(uri, json=data)

    def _fetch_pubkey(self):
        uri = self.INT_URL + "/fetch-pubkey"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        o = response.json()
        return o['pubkey']

    def _create_account(self, amount):
        pub, priv = self._create_keypair()
        uri = self.INT_URL + "/create-account"
        data = {"pubkey": pub, "amount": amount}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        return pub, priv

    def _fetch_account(self, pubkey, status_code = 200):
        uri = self.INT_URL + "/fetch-account"
        data = {"pubkey": pubkey}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, status_code)
        return response.json()

    #
    # Tests
    #

    def test_create_keypair(self):
        uri = self.INT_URL + "/create-keypair"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)

    def test_create_account(self):
        pub, _ = self._create_keypair()
        uri = self.INT_URL + "/create-account"
        data = {"pubkey": pub, "amount": 10}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        data = response.json()
        self.assertEqual(data, {})

    def test_top(self):
        uri = self.INT_URL + "/top"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)

    def test_add_peer(self):
        uri = self.INT_URL + "/peer"
        # valid
        data = {"ip": "46.101.103.165", "port": 8080}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        # invalid
        data = {"ip": "46.101.103165", "port": 8080}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 405)

    def test_spend(self):
        pub, _ = self._create_account(10)
        uri = self.INT_URL + "/spend"
        data = {"pubkey": pub, "amount": 5}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)

    def test_sync(self):
        uri = self.INT_URL + "/sync"
        # valid
        data = {"ip": "127.0.0.1", "port": 3020}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        # invalid
        data = {"ip": "127.0.0.1.1", "port": 3020}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 405)

    def test_mine_block(self):
        uri = self.INT_URL + "/mine_block"
        data = {"count": 2, "times": 1}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)

    def test_header(self):
        uri = self.EXT_URL + "/header/0"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json()["block_id"], 0)
        self.assertTrue("header" in response.json())

    def test_headers(self):
        uri = self.INT_URL + "/mine_block"
        data = {"count": 1, "times": 1}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)

        uri = self.EXT_URL + "/headers?block_ids=0,1"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        data = response.json()
        self.assertEqual(2, len(data))
        self.assertTrue("header" in data[0])
        self.assertTrue("block_id" in data[0])

    def test_delete_account(self):
        master = self._fetch_pubkey()
        pub1, priv1 = self._create_account(20)
        # pub, priv = self._create_keypair()
        self._set_keypair(pub1, priv1)
        sleep(1)
        pub2, _ = self._create_account(10)
        sleep(0.5)
        uri = self.INT_URL + "/delete-account"
        data = {"pubkey": pub2}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        sleep(0.5)
        self._fetch_account(master)
        self._fetch_account(pub1, status_code = 404)
        self._fetch_account(pub2)

    def _delete_account(self, pubkey):
        uri = self.INT_URL + "/delete-account"
        data = {"pubkey": pub}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
