#!/usr/bin/awk -f

@include "sha256"

function make_private_key(          s, i){
    print("press the enter key 10 times to generate randomness")
    s = ""
    for(i=1; i<=10; i++){
	print(11-i " more time")
	getline < "/dev/stdin"
	system("date +%N > temp")
	getline < "temp"
	close("temp")
	system("rm temp")
	#print($1)
	s = s $1
    }
    #print(s)
    return(hash_string(s))
}
function test(){
    print("256-bits of randomness: " make_private_key())
}
function scan_words(WORDS, db, n,       word, rest){
    if(match(WORDS, " ")){
	word = substr(WORDS, 0, RSTART)
	rest = substr(WORDS, RSTART+RLENGTH)
	db[n] = word
	return(scan_words(rest, db, n+1))
    } else {
	print("done")
	return(n)
    }
}
function make_password(db, many_words,         password) {
    print("using the time in nanoseconds when you press the button to generate randomness.")
    password = make_password2(db, many_words, "")
    print("your password: " password)
    return(password)
}
function make_password2(db, many_words, r,      x){
    if(many_words < 1){return(substr(r, 1))}
    print("press the enter key " int(many_words / 2) " more times")
    getline < "/dev/stdin"
    system("date +%N > temp")
    getline < "temp"
    close("temp")
    system("rm temp")
    r = r " " db[1+($1 % wordlist_size)]
    x = int($1 / wordlist_size)
    r = r " " db[1+(x % wordlist_size)]
    #print(r)
    return(make_password2(db, many_words-2, r))
}

BEGIN {
    getline < "words"
    WORDS = $0
    wordlist_size = scan_words(WORDS, db, 1)
    #print(make_private_key())
    #password = make_password(db, 12)
    #print(WORDS)
    #test()
}
