import unittest
from time import sleep

import requests

OK_RESPONSE = "ok"
URL_DEV_1 = "http://localhost:3010"
URL_DEV_1_INTERNAL = "http://localhost:3011"
URL_DEV_2 = "http://localhost:3020"
URL_DEV_2_INTERNAL = "http://localhost:3021"
URL_DEV_3 = "http://localhost:3030"
URL_DEV_3_INTERNAL = "http://localhost:3031"


class TestBase(unittest.TestCase):
    session = requests.Session()

    def request_dev1(self, data, sleep=0):
        return self.request(URL_DEV_1, data, sleep)

    def request_dev1_internal(self, data, sleep=0):
        return self.request(URL_DEV_1_INTERNAL, data, sleep)

    def request_dev2(self, data, sleep=0):
        return self.request(URL_DEV_2, data, sleep)

    def request_dev2_internal(self, data, sleep=0):
        return self.request(URL_DEV_2_INTERNAL, data, sleep)

    def request_dev3(self, data, sleep=0):
        return self.request(URL_DEV_3, data, sleep)

    def request_dev3_internal(self, data, sleep=0):
        return self.request(URL_DEV_3_INTERNAL, data, sleep)

    def request(self, url, data, seconds_to_sleep):
        response = self.session.post(url, data)
        self.assertEqual(response.status_code, 200)
        sleep(seconds_to_sleep)
        return response.json()
