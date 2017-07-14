from base import ApiUser
from nose.tools import nottest

import json
@nottest
class AccountTest(ApiUser):
    def test_single(self):
        uri = "http://localhost:3012/v1/account"
        data = {
            "address": "21mfNhxQMZjaqQsg7ULtY19PnSxogHATdHx6J4kHAv1gAj",
            "amount": 10
            }
        payload = json.dumps(data)
        response = self.session.post(uri, json=data)
        self.assertEqual(response.status_code, 200)
        data = response.json()
        self.assertEqual(data, {})


