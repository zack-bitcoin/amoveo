import unittest
from time import sleep

#import requests

OK_RESPONSE = "ok"

DEV_1 = "dev_1"
DEV_1_INT = "dev_1_int"
DEV_2 = "dev_2"
DEV_2_INT = "dev_2_int"
DEV_3 = "dev_3"
DEV_3_INT = "dev_3_int"


def byteify(input):
    if isinstance(input, dict):
        return {byteify(key): byteify(value)
                for key, value in input.iteritems()}
    elif isinstance(input, list):
        return [byteify(element) for element in input]
    elif isinstance(input, unicode):
        return str(input)
    else:
        return input


class ApiUser(unittest.TestCase):
    URL_DEV_1 = "http://localhost:3010"
    URL_DEV_1_INT = "http://localhost:3011"
    URL_DEV_2 = "http://localhost:3020"
    URL_DEV_2_INT = "http://localhost:3021"
    URL_DEV_3 = "http://localhost:3030"
    URL_DEV_3_INT = "http://localhost:3031"

    def __init__(self, *args, **kwargs):
        super(ApiUser, self).__init__(*args, **kwargs)
        #self.session = requests.Session()

        self.urls = {DEV_1: self.URL_DEV_1,
                     DEV_1_INT: self.URL_DEV_1_INT,
                     DEV_2: self.URL_DEV_2,
                     DEV_2_INT: self.URL_DEV_2_INT,
                     DEV_3: self.URL_DEV_3,
                     DEV_3_INT: self.URL_DEV_3_INT}

    def mine_block(self, node, args, sleep=0):
        return self._request(node, 'mine_block', args, sleep)

    def sync(self, node, args, sleep=0):
        return self._request(node, 'sync', args, sleep)

    def header(self, node, args, sleep=0):
        return self._request(node, 'header', args, sleep)

    def headers(self, node, args, sleep=0):
        return self._request(node, 'headers', args, sleep)

    def top(self, node, args, sleep=0):
        return self._request(node, 'top', args, sleep)

    def add_peer(self, node, args, sleep=0):
        return self._request(node, 'add_peer', args, sleep)

    def create_account(self, node, args, sleep=0):
        return self._request(node, 'create_account', args, sleep)

    def spend(self, node, args, sleep=0):
        return self._request(node, 'spend', args, sleep)

    def new_channel_with_server(self, node, args, sleep=0):
        return self._request(node, 'new_channel_with_server', args, sleep)

    def channel_spend(self, node, args, sleep=0):
        return self._request(node, 'channel_spend', args, sleep)

    def lightning_spend(self, node, args, sleep=0):
        return self._request(node, 'lightning_spend', args, sleep)

    def pull_channel_state(self, node, args, sleep=0):
        return self._request(node, 'pull_channel_state', args, sleep)

    def load_key(self, node, args, sleep=0):
        return self._request(node, 'load_key', args, sleep)

    def new_key_pair(self, node, sleep=0):
        return self._request(node, 'new_key_pair', [], sleep)

    def request(self, action, node, args, sleep=0):  # seconds to sleep
        return self._request(node, action, args, sleep)

    def _request(self, node, action, args, seconds_to_sleep=0):
        url = self.urls[node]
        data = [action] + args
        print("url is ")
        print(url)
        print("data is ")
        print(data)
        response = self.session.post(url, json=byteify(data))
        self.assertEqual(response.status_code, 200)
        sleep(seconds_to_sleep)
        return response.json()
