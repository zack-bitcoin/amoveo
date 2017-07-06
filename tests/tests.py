#!/usr/bin/env python

import requests, json, time


def main(command, port):
    url = 'http://localhost:' + str(port)
    r = requests.post(url, command, {})
    return r.text
    
assert main('["test"]', 3010) == '["test_response"]'

C1 = '["create_account", "OGlqQmhFUks0anBSVHp5Y1lDZGtVTjh0MWg5UDg2YTExMWs3N0RTUDZadUht", 10]'
assert main(C1, 3011) == '["ok","ok"]'
time.sleep(0.1)

C2 = '["create_account", "MjFtZk5oeFFNWmphcVFzZzdVTHRZMTlQblN4b2dIQVRkSHg2SjRrSEF2MWdBag==", 10]'
assert main(C2, 3011) == '["ok","ok"]'
time.sleep(0.1)

C3 = '["sync", [127,0,0,1], 3030]'
assert main(C3, 3011) == '["ok",0]'
time.sleep(0.1)

C4 = '["sync", [127,0,0,1], 3020]'
assert main(C4, 3011) == '["ok",0]'
time.sleep(0.1)

C5 = '["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]'
assert main(C5, 3011) == '["ok","ok"]'
time.sleep(0.5)

C6 = '["sync", [127,0,0,1], 3030]'
assert main(C6, 3021) == '["ok",0]'
time.sleep(0.1)

C7 = '["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]'
assert main(C7, 3021) == '["ok","ok"]'
time.sleep(0.5)

C8 = '["sync", [127,0,0,1], 3030]'
assert main(C8, 3011) == '["ok",0]'

C9 = '["new_difficulty_oracle", 20, 0, 1, 7000]' #fee, start, id, difficulty
assert main(C9, 3011) == '["ok","ok"]'
time.sleep(0.1)

C10 = '["oracle_bet", 1, 3, 269]' #one higher than minimum
assert main(C10, 3011) == '["ok","ok"]'
time.sleep(0.1)

C11 = '["mine_block", 10, 1]'
assert main(C11, 3011) == '["ok","success"]'
time.sleep(2)

C12 = '["sync", [127,0,0,1], 3030]'
assert main(C12, 3011) == '["ok",0]'
time.sleep(0.1)

C13 = '["sync", [127,0,0,1], 3020]'
assert main(C13, 3011) == '["ok",0]'
time.sleep(0.1)

C14 = '["oracle_close", 1]'
assert main(C14, 3011) == '["ok","ok"]'
time.sleep(0.1)

C15 = '["new_question_oracle", 0, "aXMgMisyPTQ/", 1, 2]'
assert main(C15, 3011) == '["ok","ok"]'
time.sleep(0.1)

C16 = '["sync", [127,0,0,1], 3030]'
assert main(C16, 3011) == '["ok",0]'
time.sleep(0.1)

C17 = '["sync", [127,0,0,1], 3020]'
assert main(C17, 3011) == '["ok",0]'
time.sleep(0.1)

C18 = '["new_market", 2, 10, 10]'
assert main(C18, 3031) == '["ok","ok"]'
time.sleep(0.1)

C19 = '["trade", 1000, 1, 1, 2, 20, [127,0,0,1], 3030]'
assert main(C19, 3011) == '["ok","ok"]'
time.sleep(0.1)

C20 = '["trade", 3000, 1, 1, 2, 20, [127,0,0,1], 3030]'
assert main(C20, 3011) == '["ok","ok"]'
time.sleep(0.1)

C21 = '["trade", 6000, 2, 2, 2, 20, [127,0,0,1], 3030]'
assert main(C21, 3021) == '["ok","ok"]'
time.sleep(0.1)

C22 = '["trade", 8000, 2, 2, 2, 20, [127,0,0,1], 3030]'
assert main(C22, 3021) == '["ok","ok"]'
time.sleep(0.1)

C23 = '["market_match", 2]'
print(main(C23, 3031))

