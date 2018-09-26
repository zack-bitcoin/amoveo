from urllib2 import HTTPError
from get_request import request_ext_file
import json

def assertEqual(x, y):
    if (x != y):
        print("fail\n")

def test_well_formed_paths():
    print("get well-formed paths")
    for path in ("/wallet.html", "/rpc.js"):
        data = request_ext_file(1, path)
        assertEqual(data.getcode(), 200)

def test_malformed_paths():
    print("get malformed paths")
    for path in ("/../", "/.", "/.."):
        code = None
        try:
            data = request_ext_file(1, path)
        except HTTPError as e:
            code = e.code

        print (path, code)
        assertEqual(code, 400)

    for path in ("/etc/passwd", ):
        code = None
        try:
            data = request_ext_file(1, path)
        except HTTPError as e:
            code = e.code

        print (path, code)
        assertEqual(code, 404)


if __name__ == "__main__":
    test_well_formed_paths()
    test_malformed_paths()
