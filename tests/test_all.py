from fork import *
from header import *
from lightning import *
from market import *
from share_blocks import *
from spend import *

print("header tests")
test_single()
test_many()

print("fork tests")
test_mine_and_sync()
test_three()

print("spend tx tests")
spend_test()

print("lightning payment tests")
lightning_test()

print("channel market tests")
market_test()

print("share blocks tests")
share_blocks_test1()
share_blocks_test2()

