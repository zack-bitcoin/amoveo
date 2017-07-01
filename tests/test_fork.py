from base import TestBase


class ForkTest(TestBase):
    def test_mine_and_sync(self):
        self.request_dev1_internal('["mine_block", 10, 1]', sleep=1)
        self.request_dev2_internal('["sync", [127,0,0,1], 3010]', sleep=1)

        self.request_dev1_internal('["mine_block", 2, 1]')
        self.request_dev2_internal('["mine_block", 5, 1]', sleep=1)

        self.request_dev1_internal('["sync", [127,0,0,1], 3020]')

    def test_three(self):
        self.request_dev1_internal('["mine_block", 10, 1]', sleep=1)
        self.request_dev2_internal('["sync", [127,0,0,1], 3010]')
        self.request_dev3_internal('["sync", [127,0,0,1], 3010]', sleep=1)

        self.request_dev1_internal('["mine_block", 2, 1]')
        self.request_dev2_internal('["mine_block", 5, 1]', sleep=1)

        self.request_dev1_internal('["sync", [127,0,0,1], 3030]', sleep=1)
        self.request_dev2_internal('["sync", [127,0,0,1], 3030]', sleep=1)
        self.request_dev1_internal('["sync", [127,0,0,1], 3030]')
