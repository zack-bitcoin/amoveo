from get_request import request_ext
import json

def assertEqual(x, y):
    if (x != y):
        print("fail\n")

def test_single():
    data = request_ext(1, 'header', [0])
    assertEqual(json.loads(data)[0], "ok")

def test_many():
    data = request_ext(1, 'headers', [1, 0])
    assertEqual(json.loads(data)[0], "ok")
