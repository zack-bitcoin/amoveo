#!/usr/bin/awk -f

#for raw binary data use cat DATA | sha256sum -b

function hash_hex(x){
    #print("echo -n " x " | xxd -r -p | sha256sum -b ")
    system("echo -n " x " | xxd -r -p | sha256sum -b > " FILE)
    getline < FILE
    close(FILE)
    system("rm " FILE)
    return(substr($0,0, length($0)-3))
}

function hash_string(x) {
    print("printf \"" x "\" | sha256sum > " FILE)
    system("printf \"" x "\" | sha256sum > " FILE)
    getline < FILE
    close(FILE)
    system("rm " FILE)
    return(substr($0,0, length($0)-3))
}

BEGIN{
    FILE = "sha_temp"
    print(hash_string("electrum's update https://github.com/spesmilo/electrum/compare/8c0adcda009474cbd2482cd94cbf1e26b636bee5...1bfee7d1956ccb31778c76955683b789d1585d0c It happened because there was a way for an attacker to spam encrypted messages to a lightning node, and cause it to crash in such a way that it loses the most recent copy of the channel state."))
    #print(hash_string(""))
    #print(hash_string("abc"))
    #print(hash_hex(""))
    #print(hash_hex("abc"))
}
