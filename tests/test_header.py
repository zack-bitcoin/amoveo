from base import TestBase, OK_RESPONSE


class HeaderTest(TestBase):
    def test_single(self):
        data = self.request_dev1('["header", 0]')
        self.assertEqual(data[0], OK_RESPONSE)

    def test_many(self):
        data = self.request_dev1('["headers", 1, 0]')
        self.assertEqual(data[0], OK_RESPONSE)
