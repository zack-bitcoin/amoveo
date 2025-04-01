@include "get_request" #request/4 and assertEqual/2

function new_block(N){
    request(N, "[\"mine_block\", 0, 0, 0]", 0.2)
}
function sync_headers_then_blocks(Who, From){
    request(Who, "[\"sync\", 2, [127,0,0,1], " From "]", 0.2)
    request(Who, "[\"sync\", [127,0,0,1], " From "]", 0.2)
}
function equal_heights(){
    height1 = request(1, "[\"height\", 1]", 0.05)
    height2 = request(2, "[\"height\", 1]", 0.05)
    assertEqual(height1, height2)
    return(height1)
}

function test_mine_and_sync(){
    print("fork test mine and sync test")
    request(1, "[\"mine_block\", 1, 100000]")
    request(2, "[\"mine_block\", 1, 100000]")
    request(3, "[\"mine_block\", 1, 100000]", 1)

    request(2, "[\"add_peer\", [127,0,0,1], 3010]")
    request(2, "[\"add_peer\", [127,0,0,1], 3030]")
    request(1, "[\"add_peer\", [127,0,0,1], 3030]")
    request(1, "[\"add_peer\", [127,0,0,1], 3020]")
    request(3, "[\"add_peer\", [127,0,0,1], 3010]")
    request(3, "[\"add_peer\", [127,0,0,1], 3020]")

    request(2, "[\"sync\", [127,0,0,1], 3010]", 0.1)
    request(1, "[\"sync\", [127,0,0,1], 3020]", 0.1)
    request(1, "[\"mine_block\", 2, 100000]", 1)
    request(2, "[\"sync\", [127,0,0,1], 3010]", 1)
    request(2, "[\"mine_block\", 12, 100000]", 0)
    request(1, "[\"mine_block\", 11, 100000]", 10)
    height1 = request(1, "[\"height\", 1]", 0.05)
    height2 = request(2, "[\"height\", 1]", 0.05)
    height3 = request(3, "[\"height\", 1]", 0.05)
    print(height1 " " height2 " " height3)
    assertEqual(height1, height2)
    assertEqual(height1, height3)
}

function test_three(){
    print("fork test: sync three nodes test")
    request(1, "[\"mine_block\", 1, 1000000]", 1.5)
    request(2, "[\"sync\", [127,0,0,1], 3010]", 0.3)
    request(3, "[\"sync\", [127,0,0,1], 3010]", 0.5)
    request(2, "[\"mine_block\", 1, 1000000]")
    request(1, "[\"mine_block\", 2, 1000000]", 2)
    request(3, "[\"sync\", [127,0,0,1], 3010]", 0.3)
    request(3, "[\"sync\", [127,0,0,1], 3020]", 0.3)
    height1 = request(1, "[\"height\"]", 0.05)
    height2 = request(2, "[\"height\"]", 0.05)
    assertEqual(height1, height2)
    print("success")
}

function test_orphan_txs(){
    pub2 = "BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8="
    request(1, "[\"add_peer\", [127,0,0,1], 3020]")
    request(2, "[\"add_peer\", [127,0,0,1], 3010]")
    request(2, "[\"mine_block\", 1, 1000000]", 0.1)
    request(1, "[\"mine_block\", 10, 1000000]", 4)
    request(1, "[\"spend\", \"" pub2 "\", 1000]", 1)
    new_block(1)
    request(1, "[\"mine_block\", 2, 1000000]", 0.1)
    request(2, "[\"sync\", 2, [127,0,0,1], 3010]", 0.2)
    request(2, "[\"sync\", [127,0,0,1], 3010]", 0.2)
    request(2, "[\"mine_block\", 2, 1000000]", 0.4)
    height1 = request(1, "[\"height\", 1]", 0.05)
    height2 = request(2, "[\"height\", 1]", 0.05)
    print(height1)
    print(height2)
    sync_headers_then_blocks(1, 3020)
    sync_headers_then_blocks(2, 3010)
#    request(1, "[\"sync\", 2, [127,0,0,1], 3020]", 0.2)
#    request(1, "[\"sync\", [127,0,0,1], 3020]", 0.2)
#    request(2, "[\"sync\", 2, [127,0,0,1], 3010]", 2)
#    request(2, "[\"sync\", [127,0,0,1], 3010]", 2)

    print("orphan tx 1")
    height1 = equal_heights()
#    height1 = request(1, "[\"height\", 1]", 0.05)
#    height2 = request(2, "[\"height\", 1]", 0.05)
#    assertEqual(height1, height2)
#    print("orphan tx 1.5")
    print(height1)

    #remove peers
    request(1, "[\"add_peer\", 0, 0]", 0.5)
    request(2, "[\"add_peer\", 0, 0]", 0.5)

    print("orphan tx 2")

    request(1, "[\"spend\", \"" pub2 "\", 1]", 1)
    request(1, "[\"spend\", \"" pub2 "\", 1]", 1)
    #request(1, "[\"txs\", [127,0,0,1], 3020]", 1)
    #exit("here") #lets see server 2's view of account pub2

    request(2, "[\"spend\", \"" pub2 "\", 10]", 0.1)
    request(2, "[\"spend\", \"" pub2 "\", 10]", 0.1)
    request(2, "[\"spend\", \"" pub2 "\", 10]", 0.1)
    #request(2, "[\"spend\", " pub2 ", 1]", 0.1)
    
    print("orphan tx 2.2")
    request(2, "[\"mine_block\"]", 0.1)
    request(1, "[\"mine_block\"]", 0.5)
    request(1, "[\"mine_block\"]", 0.1)
    height1 = request(1, "[\"height\", 1]", 0.05)
    height2 = request(2, "[\"height\", 1]", 0.05)
    print(height1)#10
    print(height2)#10


    #THIS IS WHERE THE TX GETS DROPPED
    exit("here")
    print("orphan tx 3")
    request(2, "[\"sync\", 2, [127,0,0,1], 3010]", 0.5)
    request(2, "[\"sync\", [127,0,0,1], 3010]", 0.3)
    request(1, "[\"sync\", [127,0,0,1], 3020]", 0.3)
    height1 = request(1, "[\"height\", 1]", 0.05)
    height2 = request(2, "[\"height\", 1]", 0.05)
    assertEqual(height1, height2)
    request(1, "[\"mine_block\", 1, 1000000]", 0.5)
    request(2, "[\"sync\", 2, [127,0,0,1], 3010]", 0.5)
    request(2, "[\"sync\", [127,0,0,1], 3010]", 0.3)
    height1 = request(1, "[\"height\", 1]", 0.05)
    height2 = request(2, "[\"height\", 1]", 0.05)
    assertEqual(height1, height2)
    print("orphan tx 8")
    a = request(1, "[\"account\", " pub2 "]", 0.5)
    print(a)
    print("orphan tx test finished")
}

BEGIN{
    #test_mine_and_sync()
    #test_three()
    test_orphan_txs()
}
