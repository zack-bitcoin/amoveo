from base import TestBase, OK_RESPONSE


class HeaderTest(TestBase):
    def test_single(self):
        payload = '["header", 0]'
        response = self.session.post(self.uri, data=payload)
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json()[0], OK_RESPONSE)

    def test_many(self):
        payload = '["headers", 1, 0]'
        response = self.session.post(self.uri, data=payload)
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json()[0], OK_RESPONSE)
