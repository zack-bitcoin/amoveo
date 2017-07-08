import unittest
from time import sleep

import requests

OK_RESPONSE = "ok"

DEV_1 = "dev_1"
DEV_1_INT = "dev_1_int"
DEV_2 = "dev_2"
DEV_2_INT = "dev_2_int"
DEV_3 = "dev_3"
DEV_3_INT = "dev_3_int"


class ApiUser(unittest.TestCase):
    URL_DEV_1 = "http://localhost:3010"
    URL_DEV_1_INT = "http://localhost:3011"
    URL_DEV_2 = "http://localhost:3020"
    URL_DEV_2_INT = "http://localhost:3021"
    URL_DEV_3 = "http://localhost:3030"
    URL_DEV_3_INT = "http://localhost:3031"

    MINE_BLOCK = 'mine_block'
    SYNC = 'sync'
    HEADER = 'header'
    HEADERS = 'headers'
    TOP = 'top'
    ADD_PEER = 'add_peer'
    CREATE_ACCOUNT = 'create_account'
    SPEND = 'spend'
    NEW_CHANNEL_WITH_SERVER = 'new_channel_with_server'
    CHANNEL_SPEND = 'channel_spend'
    LIGHTNING_SPEND = 'lightning_spend'
    PULL_CHANNEL_STATE = 'pull_channel_state'

    def __init__(self, *args, **kwargs):
        super(ApiUser, self).__init__(*args, **kwargs)
        self.session = requests.Session()

        self.urls = {DEV_1: self.URL_DEV_1,
                     DEV_1_INT: self.URL_DEV_1_INT,
                     DEV_2: self.URL_DEV_2,
                     DEV_2_INT: self.URL_DEV_2_INT,
                     DEV_3: self.URL_DEV_3,
                     DEV_3_INT: self.URL_DEV_3_INT}

    def mine_block(self, node, args, sleep=0):
        return self._request(node, self.MINE_BLOCK, args, sleep)

    def sync(self, node, args, sleep=0):
        return self._request(node, self.SYNC, args, sleep)

    def header(self, node, args, sleep=0):
        return self._request(node, self.HEADER, args, sleep)

    def headers(self, node, args, sleep=0):
        return self._request(node, self.HEADERS, args, sleep)

    def top(self, node, args, sleep=0):
        return self._request(node, self.TOP, args, sleep)

    def add_peer(self, node, args, sleep=0):
        return self._request(node, self.ADD_PEER, args, sleep)

    def create_account(self, node, args, sleep=0):
        return self._request(node, self.CREATE_ACCOUNT, args, sleep)

    def spend(self, node, args, sleep=0):
        return self._request(node, self.SPEND, args, sleep)

    def new_channel_with_server(self, node, args, sleep=0):
        return self._request(node, self.NEW_CHANNEL_WITH_SERVER, args, sleep)

    def channel_spend(self, node, args, sleep=0):
        return self._request(node, self.CHANNEL_SPEND, args, sleep)

    def lightning_spend(self, node, args, sleep=0):
        return self._request(node, self.LIGHTNING_SPEND, args, sleep)

    def pull_channel_state(self, node, args, sleep=0):
        return self._request(node, self.PULL_CHANNEL_STATE, args, sleep)

    def _request(self, node, action, args, seconds_to_sleep):
        url = self.urls[node]
        data = str([action] + args)
        data = data.replace("\'", "\"")

        response = self.session.post(url, data)
        self.assertEqual(response.status_code, 200)
        sleep(seconds_to_sleep)
        return response.json()
