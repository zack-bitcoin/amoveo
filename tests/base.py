import unittest

import requests

OK_RESPONSE = "ok"


class TestBase(unittest.TestCase):
    session = requests.Session()
    uri = "http://localhost:3010"
