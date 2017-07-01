from time import sleep

from base import TestBase


class ShareBlocksTest(TestBase):
    def test(self):
        response = self.session.post("http://localhost:3021",
                                     data='["add_peer", [127,0,0,1], 3010]')
        self.assertEqual(response.status_code, 200)
        response = self.session.post("http://localhost:3011",
                                     data='["add_peer", [127,0,0,1], 3020]')
        self.assertEqual(response.status_code, 200)

        response = self.session.post("http://localhost:3011",
                                     data='["mine_block", 5, 1]')
        self.assertEqual(response.status_code, 200)
        sleep(1)

        response = self.session.post("http://localhost:3011",
                                     data='["sync", [127,0,0,1], 3020]')
        self.assertEqual(response.status_code, 200)
        sleep(1)

        response = self.session.post("http://localhost:3021",
                                     data='["mine_block", 5, 1]')
        self.assertEqual(response.status_code, 200)
        sleep(1)

        response = self.session.post("http://localhost:3011",
                                     data='["sync", [127,0,0,1], 3020]')
        self.assertEqual(response.status_code, 200)
        response = self.session.post("http://localhost:3011",
                                     data='["mine_block", 5, 1]')
        self.assertEqual(response.status_code, 200)
        sleep(1)

        response = self.session.post("http://localhost:3011",
                                     data='["sync", [127,0,0,1], 3020]')
        self.assertEqual(response.status_code, 200)
