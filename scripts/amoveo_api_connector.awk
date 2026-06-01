#!/bin/awk -f

function internal(n){
    return(call_api(n, "localhost", 8081))
}
function external(n){
    return(call_api(n, "localhost", 8080))
}
function call_api(n, ip, port){
    n=put_quotes_on_words(n)
    x = system("curl -s -d '[" n "]' http://" ip ":" port " -o temp_file")
    file = "temp_file"
    getline var < file
    close(file)
    system("rm temp_file")
    var = substr(var, 7)
    var = substr(var, 0, length(var)-1)
    return(var)
}
function put_quotes_on_words(s){
    N = split(s, a, ",")
    r = ""
    for(i=1; i<=N; i++){
	if(a[i] ~ /[0-9]+/){#numeric, so no quotes needed.
	    r = r "," a[i]
        } else {
            r = r ",\"" a[i] "\""
	}
    }
    return(substr(r, 2))#remove leading comma
}

function halt(){
    return(internal("halt"))
}
function header_height(){
    return(internal("height"))
}
function block_height(){
    return(internal( "height, 1"))
}
function block_bottom(){
    return(internal("height, 3"))
}
function key_status(){
    return(internal("keys_status"))
}
function txs(n){
    return(internal("block, 3, "n))
}
function blocks(start, end){
    return(internal("blocks, "start", "end))
}
function top_header(){
    return(internal("top"))
}
function top_with_block(){
    return(internal("top, 1"))
}
function sign(tx){
    return(internal("sign, "tx))
}
function spend(addr, amount){
    return(internal("spend, "addr", "amount))
}
function my_account(){
    return(internal("account"))
}
function account(N){
    return(internal("account, "N))
}
function confirmed_balance(A){
    return(internal("confirmed_balance, "A))
}
function balance(){
    return(internal("balance"))
}
function off(){
    return(internal("off"))
}
function sync_check(){
    return(internal("sync"))
}
function sync_normal(){
    return(internal("sync_normal"))
}
function sync_quick(){
    return(internal("sync_quick"))
}
function pubkey(){
    return(internal("pubkey"))
}
BEGIN {
    H = block_height()
    print("block height: " H)
    print("header height:  " header_height())
    print("block bottom: " block_bottom())
    print("keys status: " key_status())
    print(txs(H))
    print("total work: " external("height, 4"))
    print("done")
}
