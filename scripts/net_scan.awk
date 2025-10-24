function int_command(H, Peer,     x, ln, line, s, Ins) {
    Ins = "curl -s -i -d " H " " Peer " -o temp"
    x = system(Ins)
    #system("cat temp")
    ln = 0
    while(getline line < "temp") {
        ln += 1
        if(ln == 8){
            match(line, /[0-9]+/)
            s = substr(line, RSTART, RLENGTH)
            close("temp")
            system("rm temp")
            return(s)
        }
    }
}
function command(H, Peer,     x, ln, line, s, Ins) {
    Ins = "curl -s -i -d " H " " Peer " -o temp"
    x = system(Ins)
    #system("cat temp")
    ln = 0
    while(getline line < "temp") {
        ln += 1
        if(ln == 8){
            match(line, /,[0-9][0-9][0-9][0-9]+\]/)
            s = substr(line, RSTART+1, RLENGTH-2)
            close("temp")
            system("rm temp")
            return(s)
        }
    }
}


BEGIN {
    Pool[1] = "http://159.223.85.216:8085"
    Pool[2] = "http://159.65.126.146:8085"
    Pool[3] = "http://43.133.42.108:8085"
    Pool[4] = "http://31.130.148.203:8085"
    Peer[1] = "http://159.223.85.216:8080"
    Peer[2] = "http://159.65.126.146:8080"
    Peer[3] = "http://43.133.42.108:8080"
    Peer[4] = "http://31.130.148.203:8080"
    Peer[5] = "http://64.227.21.70:8080"
    Peer[6] = "http://46.101.81.5:8080"
    Ids[1] = "pool singapore"
    Ids[2] = "pool germany"
    Ids[3] = "pool china"
    Ids[4] = "pool russia"
    Ids[5] = "explorer 1"
    Ids[6] = "explorer 2"
    #Peer[6] = "http://43.163.6.249:8080"
    #{peers, [{{159,223,85,216},8080},{{159,65,126,146},8080},{{64,227,21,70},8080},{{46,101,81,5},8080},{{43,133,42,108},8080}]},\
    #for(i=1; i<=5; i++){
    for(i in Pool){
        print("pool id: " Ids[i] " url: " Pool[i])
        print("status: " command("'[\"mining_data\"]'", Pool[i]))
    }
    for(i in Peer){
        print("id: " Ids[i] " url: " Peer[i])
        print("header height: " int_command("'[\"height\", 1]'", Peer[i]))
        print("block height: " int_command("'[\"height\"]'", Peer[i]))
        print("block bottom: " int_command("'[\"height\", 3]'", Peer[i]))
        print("total work done: " (int(int_command("'[\"height\", 4]'", Peer[i])/100000000000000)))
        print("")
        #print(command("'[\"height\", 1]'", Peer[i]))
    }
}
