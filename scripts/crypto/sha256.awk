#!/usr/bin/awk -f

#for raw binary data use cat DATA | sha256sum -b

function hash_hex(x){
    system("echo -n " x " | xxd -r -p | sha256sum -b > " FILE)
    getline < FILE
    close(FILE)
    system("rm " FILE)
    return(substr($0,0, length($0)-3))
}

function hash_string(x) {
    system("printf \"" x "\" | sha256sum > " FILE)
    getline < FILE
    close(FILE)
    system("rm " FILE)
    return(substr($0,0, length($0)-3))
}

BEGIN{
    FILE = "sha_temp"
    #print(hash_string(""))
    #print(hash_string("abc"))
    #print(hash_hex(""))
    #print(hash_hex("abc"))
}
