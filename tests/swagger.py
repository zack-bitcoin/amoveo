import unittest
import requests

from time import sleep

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

class SwaggerTest(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super(SwaggerTest, self).__init__(*args, **kwargs)

    def c(self, tup, expected = 200):
        # 0 is assumed to be the status code
        if isinstance(tup, tuple):
            self.assertEqual(tup[0], expected)
            # unwrap 1-element tuples
            if len(tup) == 2:
                return tup[1]
            else:
                return tup[1:]
        else:
            self.assertEqual(tup, expected)
            return None

class API(object):
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.URL = 'http://' + self.host + ":" + str(self.port) + "/v1"
        self.session = requests.Session()

    def request(self, action, node, args, sleep=0):  # seconds to sleep
        return self._request(node, action, args, sleep)

    def _request(self, node, action, args, seconds_to_sleep=0):
        url = self.urls[node]
        data = [action] + args
        r = self.session.post(url, json=byteify(data))
        self.assertEqual(r.status_code, 200)
        sleep(seconds_to_sleep)
        return r.json()

class IntAPI(API):
    def __init__(self, *args, **kwargs):
        super(IntAPI, self).__init__(*args, **kwargs)

    def create_keypair(self):
        uri = self.URL + '/create-keypair'
        r = self.session.get(uri)
        code = r.status_code
        if code != 200:
            return code, None, None
        o = r.json()
        return code, o['public'], o['private']

    def fetch_keypair(self):
        uri = self.URL + '/fetch-keypair'
        r = self.session.get(uri)
        code = r.status_code
        if code != 200:
            return code, None, None
        o = r.json()
        return code, o['public'], o['private']

    def set_keypair(self, public, private, brainwallet = ''):
        uri = self.URL + '/set-keypair'
        data = {
            'public': public,
            'private': private,
            'brain-wallet': brainwallet
        }
        r = self.session.post(uri, json=data)
        return r.status_code

    def fetch_pubkey(self):
        uri = self.URL + '/fetch-pubkey'
        r = self.session.get(uri)
        code = r.status_code
        if code != 200:
            return code, None
        o = r.json()
        return code, o['pubkey']

    def create_account(self, amount):
        code, pub, priv = self.create_keypair()
        if code != 200:
            return code, None, None
        code = self.create_account_with_key(pub, amount)
        if code != 200:
            return code, None, None
        return code, pub, priv

    def create_account_with_key(self, pub, amount):
        uri = self.URL + '/create-account'
        data = {'pubkey': pub, 'amount': amount}
        r = self.session.post(uri, json=data)
        code = r.status_code
        if code != 200:
            return code, None, None
        return code

    def fetch_account(self, pubkey):
        uri = self.URL + '/fetch-account'
        data = {'pubkey': pubkey}
        r = self.session.post(uri, json=data)
        code = r.status_code
        if code != 200:
            return code, None
        # XXX return instance of account
        return code, r.json()

    def delete_account(self, pubkey):
        uri = self.URL + '/delete-account'
        data = {'pubkey': pub}
        r = self.session.post(uri, json=data)
        return r.status_code

    def top(self):
        uri = self.URL + '/top'
        r = self.session.get(uri)
        code = r.status_code
        if code != 200:
            return None
        o = r.json()
        return code, o['hash'], o['height']

    def add_peer(self, address, port):
        uri = self.URL + '/peer'
        data = {'ip': address, 'port': port}
        r = self.session.post(uri, json=data)
        return r.status_code

    def spend(self, pubkey, amount):
        uri = self.URL + '/spend'
        data = {'pubkey': pubkey, 'amount': amount}
        r = self.session.post(uri, json=data)
        return r.status_code

    def sync(self, address, port):
        uri = self.URL + '/sync'
        data = {'ip': address, 'port': port}
        r = self.session.post(uri, json=data)
        return r.status_code

    def mine_block(self, count, times):
        uri = self.URL + '/mine-block'
        data = {'count': count, 'times': times}
        r = self.session.post(uri, json=data)
        return r.status_code

    def delete_account(self, pubkey):
        uri = self.URL + '/delete-account'
        data = {'pubkey': pubkey}
        r = self.session.post(uri, json=data)
        return r.status_code

    def new_channel_with_server(self, ip, port, chan_id, balance, recv_limit, fee, delay):
        uri = self.URL + '/new-channel-with-server'
        data = {
            'ip': ip,
            'port': port,
            'channel-id': chan_id,
            'balance': balance,
            'receive-limit': recv_limit,
            'fee': fee,
            'delay': delay
        }
        r = self.session.post(uri, json=data)
        return r.status_code

    def channel_spend(self, address, port, amount):
        uri = self.URL + '/channel-spend'
        data = {'ip': address, 'port': port, 'amount': amount}
        r = self.session.post(uri, json=data)
        return r.status_code

    def lightning_spend(self, address, port, pubkey, amount, fee):
        uri = self.URL + '/lightning-spend'
        data = {
            'ip': address,
            'port': port,
            'pubkey': pubkey,
            'amount': amount,
            'fee': fee
        }
        r = self.session.post(uri, json=data)
        return r.status_code

    def pull_channel_state(self, address, port):
        uri = self.URL + '/pull-channel-state'
        data = {'ip': address, 'port': port}
        r = self.session.post(uri, json=data)
        return r.status_code


class ExtAPI(API):
    def __init__(self, *args, **kwargs):
        super(ExtAPI, self).__init__(*args, **kwargs)

    def header(self, blockid):
        uri = self.URL + "/header"
        data = {'block-id': blockid}
        r = self.session.post(uri, json=data)
        code = r.status_code
        if code != 200:
            return code, None
        o = r.json()
        return code, o['block-id'], o['header']

    def headers(self, block_ids):
        uri = self.URL + "/headers"
        data = []
        for block_id in block_ids:
            data.append({'block-id': block_id})
        r = self.session.post(uri, json=data)
        code = r.status_code
        if code != 200:
            return code, None
        return code, r.json()

    def channel_sync(self, pubkey, sig):
        uri = self.URL + "/channel-sync"
        data = {'pubkey': pubkey, 'sig': sig}
        r = self.session.post(uri, json=data)
        code = r.status_code
        if code != 200:
            return code, None
        return code, r.json()
