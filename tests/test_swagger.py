from base import ApiUser


class SwaggerTest(ApiUser):
    INTERNAL_URL = "http://localhost:3012/v1"
    EXTERNAL_URL = "http://localhost:3013/v1"

    def test_new_keypair(self):
        uri = self.INTERNAL_URL + "/keypair"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)

    def new_pubkey(self):
        uri = self.INTERNAL_URL + "/keypair"
        response = self.session.get(uri)
        o = response.json()
        return o['public']

    def test_account(self):
        pub = self.new_pubkey()
        uri = self.INTERNAL_URL + "/account"
        data = {"pubkey": pub, "amount": 10}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        data = response.json()
        self.assertEqual(data, {})

    def test_top(self):
        uri = self.INTERNAL_URL + "/top"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)

    def test_add_peer(self):
        uri = self.INTERNAL_URL + "/peer"
        # valid
        data = {"ip": "46.101.103.165", "port": 8080}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        # invalid
        data = {"ip": "46.101.103165", "port": 8080}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 405)

    def _create_account(self, amount):
        pub = self.new_pubkey()
        uri = self.INTERNAL_URL + "/account"
        data = {"pubkey": pub, "amount": amount}
        self.session.post(uri, json=data)
        return pub

    def test_spend(self):
        pub = self._create_account(10)
        uri = self.INTERNAL_URL + "/spend"
        data = {"pubkey": pub, "amount": 5}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)

    def load_keypair(self, public, private, brainwallet):
        uri = self.INTERNAL_URL + "/load-keypair"
        data = {
            "public": public,
            "private": private,
            "brain-wallet": brainwallet
        }
        self.session.post(uri, json=data)

    def test_sync(self):
        uri = self.INTERNAL_URL + "/sync"
        # valid
        data = {"ip": "127.0.0.1", "port": 3020}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        # invalid
        data = {"ip": "127.0.0.1.1", "port": 3020}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 405)

    def test_mine_block(self):
        uri = self.INTERNAL_URL + "/mine_block"
        data = {"count": 2, "times": 1}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)

    def test_header(self):
        uri = self.EXTERNAL_URL + "/header/0"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json()["block_id"], 0)
        self.assertTrue("header" in response.json())

    def test_headers(self):
        uri = self.INTERNAL_URL + "/mine_block"
        data = {"count": 1, "times": 1}
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)

        uri = self.EXTERNAL_URL + "/headers?block_ids=0,1"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)
        data = response.json()
        self.assertEqual(2, len(data))
        self.assertTrue("header" in data[0])
        self.assertTrue("block_id" in data[0])
