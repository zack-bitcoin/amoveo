from base import TestBase, OK_RESPONSE


class ServerCheckTest(TestBase):
    def test(self):
        response = self.session.post("http://localhost:3010", data='["top"]')
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json()[0], OK_RESPONSE)
