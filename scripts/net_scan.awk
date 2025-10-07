
    #{peers, [{{159,223,85,216},8080},{{159,65,126,146},8080},{{64,227,21,70},8080},{{46,101,81,5},8080},{{43,133,42,108},8080}]},\

function command(H, Peer,     x, ln, line, s) {
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


BEGIN {
    Peer[1] = "http://159.223.85.216:8080"
    Peer[3] = "http://159.65.126.146:8080"
    Peer[2] = "http://64.227.21.70:8080"
    Peer[4] = "http://46.101.81.5:8080"
    Peer[5] = "http://43.133.42.108:8080"
    #{peers, [{{159,223,85,216},8080},{{159,65,126,146},8080},{{64,227,21,70},8080},{{46,101,81,5},8080},{{43,133,42,108},8080}]},\
    for(i=1; i<6; i++){
        print("peer: " Peer[i])
        print("block height: " command("'[\"height\"]'", Peer[i]))
        print("header height: " command("'[\"height\", 1]'", Peer[i]))
        print("")
        #print(command("'[\"height\", 1]'", Peer[i]))
    }
}
