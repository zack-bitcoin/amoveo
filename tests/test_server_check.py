from base import ApiUser, DEV_1, OK_RESPONSE
from nose.tools import nottest

@nottest
class ServerCheckTest(ApiUser):
    def test(self):
        data = self.top(DEV_1, [])
        self.assertEqual(data[0], OK_RESPONSE)
