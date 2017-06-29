# start the two servers:
# sh start.sh 3010
# sh start.sh 3020


from time import sleep

from base import TestBase


class ForkTest(TestBase):
    def test_mine_and_sync(self):
        response = self.session.post("http://localhost:3011",
                                     data='["mine_block", 10, 1]')
        self.assertEqual(response.status_code, 200)
        sleep(1)
        response = self.session.post("http://localhost:3021",
                                     data='["sync", [127,0,0,1], 3010]')
        self.assertEqual(response.status_code, 200)
        sleep(1)

        response = self.session.post("http://localhost:3011",
                                     data='["mine_block", 2, 1]')
        self.assertEqual(response.status_code, 200)
        response = self.session.post("http://localhost:3021",
                                     data='["mine_block", 5, 1]')
        self.assertEqual(response.status_code, 200)
        sleep(1)

        response = self.session.post("http://localhost:3011",
                                     data='["sync", [127,0,0,1], 3020]')
        self.assertEqual(response.status_code, 200)
