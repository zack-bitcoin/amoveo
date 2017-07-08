import requests, json, time


def main2(command, port):
    return main(command, port, 0.1)
def main(command, port, t):
    url = 'http://localhost:' + str(port)
    r = requests.post(url, command, {})
    time.sleep(t)
    return r.text
    
#assert main2('["test"]', 3010) == '["test_response"]'

C1 = '["create_account", "OGlqQmhFUks0anBSVHp5Y1lDZGtVTjh0MWg5UDg2YTExMWs3N0RTUDZadUht", 10]'
#assert main2(C1, 3011) == '["ok","ok"]'

C2 = '["create_account", "MjFtZk5oeFFNWmphcVFzZzdVTHRZMTlQblN4b2dIQVRkSHg2SjRrSEF2MWdBag==", 10]'
#assert main2(C2, 3011) == '["ok","ok"]'

C3 = '["sync", [127,0,0,1], 3030]'
#assert main2(C3, 3011) == '["ok",0]'

C4 = '["sync", [127,0,0,1], 3020]'
#assert main2(C4, 3011) == '["ok",0]'

C5 = '["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]'
#assert main(C5, 3011, 0.5) == '["ok","ok"]'

C6 = '["sync", [127,0,0,1], 3030]'
#assert main2(C6, 3021) == '["ok",0]'

C7 = '["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]'
#assert main(C7, 3021, 0.5) == '["ok","ok"]'

C8 = '["sync", [127,0,0,1], 3030]'
#assert main2(C8, 3011) == '["ok",0]'

C9 = '["new_difficulty_oracle", 20, 0, 1, 7000]' #fee, start, id, difficulty
#assert main2(C9, 3011) == '["ok","ok"]'

C10 = '["oracle_bet", 1, 3, 269]' #one higher than minimum
#assert main2(C10, 3011) == '["ok","ok"]'

C11 = '["mine_block", 10, 1]'
#assert main2(C11, 3011) == '["ok","success"]'

C12 = '["sync", [127,0,0,1], 3030]'
#assert main2(C12, 3011) == '["ok",0]'

C13 = '["sync", [127,0,0,1], 3020]'
#assert main2(C13, 3011) == '["ok",0]'

C14 = '["oracle_close", 1]'
#assert main2(C14, 3011) == '["ok","ok"]'

C15 = '["new_question_oracle", 0, "aXMgMisyPTQ/", 1, 2]'
#assert main2(C15, 3011) == '["ok","ok"]'

C16 = '["sync", [127,0,0,1], 3030]'
#assert main2(C16, 3011) == '["ok",0]'

C17 = '["sync", [127,0,0,1], 3020]'
#assert main2(C17, 3011) == '["ok",0]'

C18 = '["new_market", 2, 10, 10]'
#assert main2(C18, 3031) == '["ok","ok"]'

C19 = '["trade", 1000, 1, 1, 2, 20, [127,0,0,1], 3030]'
#assert main2(C19, 3011) == '["ok","ok"]'

C20 = '["trade", 3000, 1, 1, 2, 20, [127,0,0,1], 3030]'
#assert main2(C20, 3011) == '["ok","ok"]'

C21 = '["trade", 6000, 2, 2, 2, 20, [127,0,0,1], 3030]'
#assert main2(C21, 3021) == '["ok","ok"]'

C22 = '["trade", 8000, 2, 2, 2, 20, [127,0,0,1], 3030]'
#assert main2(C22, 3021) == '["ok","ok"]'

C23 = '["market_match", 2]'
#assert main2(C23, 3031) == '["ok","ok"]'

C24 = '["oracle_bet", 2, 1, 269]'
#assert main2(C24, 3011) == '["ok","ok"]'

C25 = '["mine_block", 10, 1]'
#assert main2(C25, 3011) == '["ok","success"]'

C26 = '["oracle_close", 2]'
#assert main2(C26, 3011) == '["ok","ok"]'

C27 = '["sync", [127,0,0,1], 3030]'
#assert main2(C27, 3011) == '["ok",0]'
#assert main2(C27, 3011) == '["ok",0]'

C28 = '["sync", [127,0,0,1], 3020]'
#assert main2(C28, 3011) == '["ok",0]'
#assert main2(C28, 3011) == '["ok",0]'

#oracle better collects winnings in both oracles
C29 = '["oracle_shares", 1]'
#assert main2(C29, 3011) == '["ok","ok"]'

C30 = '["oracle_shares", 2]'
#assert main2(C30, 3011) == '["ok","ok"]'

C31 = '["oracle_unmatched", 1, 2]' #order id wrong
#assert main2(C31, 3011) == '["ok","ok"]'

C32 = '["oracle_unmatched", 2, 2]'
#assert main2(C32, 3011) == '["ok","ok"]'

C33 = '["pull_channel_state", [127,0,0,1], 3030]'
#print(main2(C33, 3011))


#curl -i -d '["pull_channel_state", [127,0,0,1], 3030]' http://localhost:3021

#unmatched trades from off-chain market should be undone.
#winners in off-chain market should collect their winnings.
#losers in the off-chain market should update their channel to reflect the new state.
