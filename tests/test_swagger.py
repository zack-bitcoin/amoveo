from time import sleep
from base import ApiUser


class SwaggerTest(ApiUser):
    INT_URL = "http://localhost:3012/v1"
    EXT_URL = "http://localhost:3013/v1"

    def test_create_keypair(self):
        uri = self.INT_URL + "/create-keypair"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)

    def _create_keypair(self):
        uri = self.INT_URL + "/create-keypair"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        o = response.json()
        return o['public'], o['private']

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

    def _create_account(self, amount):
        pub, _ = self._create_keypair()
        uri = self.INT_URL + "/create-account"
        data = {"pubkey": pub, "amount": amount}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        return pub

    def test_spend(self):
        pub = self._create_account(10)
        uri = self.INT_URL + "/spend"
        data = {"pubkey": pub, "amount": 5}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)

    def _set_keypair(self, public, private, brainwallet = ''):
        uri = self.INT_URL + "/set-keypair"
        data = {
            "public": public,
            "private": private,
            "brain-wallet": brainwallet
        }
        self.session.post(uri, json=data)

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
        self.assertTrue("header" in response.json())

    def _fetch_account(self, pubkey, status_code = 200):
        uri = self.INT_URL + "/fetch-account"
        data = {"pubkey": pubkey}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, status_code)
        return response.json()

    def _fetch_pubkey(self):
        uri = self.INT_URL + "/fetch-pubkey"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        o = response.json()
        return o['pubkey']

    # def test_delete_account(self):
    #     master = self._fetch_pubkey()
    #     pub, priv = self._create_keypair()
    #     self._set_keypair(pub, priv)
    #     sleep(1)
    #     pub1 = self._create_account(20)
    #     pub2 = self._create_account(20)
    #     sleep(0.5)
    #     uri = self.INT_URL + "/delete-account"
    #     data = {"pubkey": pub1}
    #     response = self.session.post(uri, json=data)
    #     self.assertEqual(response.status_code, 200)
    #     sleep(0.5)
    #     self._fetch_account(master)
    #     self._fetch_account(pub1, status_code = 404)
    #     self._fetch_account(pub2)

    def _delete_account(self, pubkey):
        uri = self.INT_URL + "/delete-account"
        data = {"pubkey": pub}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
