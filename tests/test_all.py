from fork import *
from header import *
from lightning import *
from market import *
from share_blocks import *
from spend import *
from ext_handler import *


test_header_single()
test_header_many()

test_well_formed_paths()
test_malformed_paths()

test_mine_and_sync()
test_three()

spend_test()

lightning_test()

market_test()

share_blocks()
print("finished tests\n")
