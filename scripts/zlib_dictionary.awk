#zlib compression is able to accept a dictionary to help make the compression more efficient. The purpose of this awk program is to accept some data that will be compressed, and to generate a zlib dictionary for that data to compress it efficiently.
#input is all on one line.

function add_if_big_enough(s) {
    if(length(s) > 1){
        Words[s] += 1
    }
}

function make_words_from_skipped_part(skip){
    while(match(skip, /([0-9]{2,}|,)/)){
        tail = substr(skip, RSTART+RLENGTH, length(skip))
        skip2 = substr(skip,0, RSTART-1)
        add_if_big_enough(skip2)
        skip = tail
    }
    add_if_big_enough(skip)
    return(0)
}


BEGIN {
   RS = "fsfhsdhflsflsdlfsdafjsjfsldjflsone line"
}

{
    original_string = $0
    s = $0
    reg = "\\\\\"[a-zA-Z0-9=+/]{20,}\\\\\""
    while(match(s, reg)){
        skip = substr(s, 0, RSTART)
        #print("skipped part: " skip)
        a = substr(s, RSTART, RLENGTH)
        if(1){#length(a) < 98){
            Words[a] += 1
        } else {
            verkle_part += length(a)
        }
        #print("before remove: " s)
        s = substr(s, RSTART, length(s))
        #print("after remove: " s)
        sub(reg, "", s)
        make_words_from_skipped_part(skip)
    }
    make_words_from_skipped_part(s)
}

END {
    #print("in end")
    for(w in Words){
        if(length(w) == 48){
            prev_waste += 53
        } else if((Words[w] > 1)){
            print("word: " length(w) " " Words[w] " " w)
            size += length(w)
            savings += length(w) * (Words[w]-1)
        }
    }
    print("raw size - verkle part: " length(original_string) - verkle_part - prev_waste)
    print("dictionary size: " size)
    print("dictionary portion: " size / (length(original_string) - verkle_part - prev_waste))
    print("bytes saved: " savings)
}
#1 block
#raw size: 3784
#dictionary size: 455
#dictionary portion: 0.120243
#bytes saved: 583

#9 blocks
#raw size: 21436
#dictionary size: 1469
#dictionary portion: 0.0685296
#bytes saved: 4892

#50 blocks
#raw size: 145301
#dictionary size: 7402
#dictionary portion: 0.0509425
#bytes saved: 45938

#224 blocks
#raw size: 580601
#dictionary size: 11990
#dictionary portion: 0.020651
#bytes saved: 160190

