from time import sleep
from swagger import SwaggerTest, IntAPI, ExtAPI

class ExternalAPITest(SwaggerTest):
    INT_API = {
        'dev1': IntAPI('localhost', 3012),
        'dev2': IntAPI('localhost', 3022),
        'dev3': IntAPI('localhost', 3032),
    }
    API = {
        'dev1': ExtAPI('localhost', 3013),
        'dev1': ExtAPI('localhost', 3023),
        'dev1': ExtAPI('localhost', 3033)
    }
    URL = {
        'dev1': 'http://localhost:3013/v1',
        'dev2': 'http://localhost:3023/v1',
        'dev3': 'http://localhost:3033/v1'
    }

    def test_header(self):
        api = self.API['dev1']
        block_id, header = self.c(api.header(0))
        self.assertEqual(block_id, 0)
        self.assertIsNotNone(header)

    def test_headers(self):
        int_api = self.INT_API['dev1']
        self.c(int_api.mine_block(1, 1))
        api = self.API['dev1']
        a = self.c(api.headers([0, 1]))
        self.assertEqual(len(a), 2)

