from base import ApiUser, DEV_1, OK_RESPONSE
from nose.tools import nottest

class HeaderTest(ApiUser):
    def test_single(self):
        data = self.header(DEV_1, [0])
        self.assertEqual(data[0], OK_RESPONSE)

    def test_many(self):
        data = self.headers(DEV_1, [1, 0])
        self.assertEqual(data[0], OK_RESPONSE)
