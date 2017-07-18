from base import ApiUser
from nose.tools import nottest

import json


class SwaggerTest(ApiUser):
    URL = "http://localhost:3012/v1"

    def test_new_keypair(self):
        uri = self.URL + "/keypair"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)

    def new_pubkey(self):
        uri = self.URL + "/keypair"
        response = self.session.get(uri)
        o = response.json()
        return o['public']

    def test_account(self):
        pub = self.new_pubkey()
        uri = self.URL + "/account"
        data = {
            "address": pub,
            "amount": 10
            }
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        data = response.json()
        self.assertEqual(data, {})

    def test_top(self):
        uri = self.URL + "/top"
        response = self.session.get(uri)
        self.assertEqual(response.status_code, 200)


