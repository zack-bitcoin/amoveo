BEGIN{
    #header, height, prev, root, txs_root, time, diff, version, nonce, a_diff, period
    #{headers, many, height}
    LOCAL = "http://localhost:8080"
    H1 = int_command("'[\"height\", 1]'", LOCAL)
    match(H1, /[0-9]+/)
    HEIGHT = substr(H1, RSTART, RLENGTH)+0
    #HEIGHT = 100000
    MAX_DIFF = 0
    print("header height: " HEIGHT)
    BATCH_SIZE = 5000
    load_headers(BATCH_SIZE, HEIGHT, DB)
    for(i=1; i<=HEIGHT; i+= 100){
        #print(i " " DB[i])
    }
    draw_chart(DB, 2000, 1000)
}
function draw_chart(DB, width, height){
    step = int(HEIGHT / width)
    j = 1
    for(i=1; i<=HEIGHT; i+=step){
        d = DB[i]
        pixel_height[j] = int(height * d / MAX_DIFF)
        j++
    }
    s = "P2\n"width " " height"\n2\n"
    for(y=height; y>=0; y--){
        for(x=0; x<width; x++){
            if(pixel_height[x] == y){
                s = s "1 "
            } else if(pixel_height[x] < y){
                s = s "2 "
            } else if(pixel_height[x] > y){
                s = s "0 "
            }
        }
        s = s "\n"
    }
    print(s) > ("chart.ppm")
    system("convert chart.ppm chart.jpg")
    system("rm chart.ppm")
}

function int_command(H, Peer) {
    file = "temp"
    Ins = "curl -s -d " H " " Peer " -o " file
    system(Ins)
    getline line < file
    close(file)
    system("rm " file)
    return(line)
}
function get_batch(many, start){
    line = int_command("'[\"headers\", " many ", " start "]'",
                       LOCAL)
    M = split(line, A, "header")
    s = ""
    for(i=2; i<=M; i++){
        N = split(A[i], B, ",")
        s = s "," sci2int(B[7])
    }
    return(substr(s, 2))
}
function load_headers(BatchSize, Height, DB,      rounds, h, i, b, M, A, j){
    rounds = int(Height / BatchSize)
    h = 1
    for(i=0; i< rounds; i++){
        b = get_batch(BatchSize, i*BatchSize)
        M = split(b, A, ",")
        for(j=1; j<=M; j++){
            DB[h] = A[j]
            MAX_DIFF = max(MAX_DIFF, A[j])
            if((h % 10000) == 0){
                print(h " " int(A[j]/10000000000000))
            }
            h += 1
        }
    }
}
function exponent(a, b){
    if(b == 0){return(1)}
    if(b == 1){return(a)}
    if((b % 2) == 0){return(exponent(a*a, int(b / 2)))}
    return(a * exponent(a, b-1))
}
function lg(x){
    if(x == 1){return(1)}
    return(1 + lg(int(x/2)))
}
function sci2int(x,    A, B){
    A = int(x / 256)
    B = x % 256
    return(int((exponent(2, A) * (256+B)) / 256))
}
function int2sci(x){
    A = lg(p) - 1
    B = ((P * 256) / exponent(2, A)) - 256
    return((256*A)+B)
}
function max(a, b){
    if(a > b){ return (a)}
    return(b)
}
