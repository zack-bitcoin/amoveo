BEGIN{
    temp_file = "temp"
}
#function request(node, command, args, sleeptime,         i){
function request(node, command, sleeptime,        i){
#    if(args == ""){
        #cmd = "curl -s -i -d '[\"" command "\"]' http://localhost:" (3000+(node*10)) " > "temp_file
    
        cmd = "curl -s -i -d '" command "' http://localhost:" (3001+(node*10)) " > "temp_file
        r = system(cmd)
        if(r == 0){
            i=1
            temp_data = ""
            while((getline  < temp_file) > 0){
                temp_data = temp_data $0 "\n"
                if(i==8){
                   # if(match($0, /\["ok",[0-9a-zA-Z,{}"'=\/\-\[\]\+ ]*]/)){
                    if(match($0, /\["ok",[a-zA-Z0-9,{}"'=\/\[\]+ \-_]*\]$/)){
                        x = substr($0, 7, length($0) - 7)
                        close(temp_file)
                        if(sleeptime){
                            system("sleep " sleeptime)
                                   }
                        return(x)
                    } else {
                        print("get_request.awk: failed to match output")
                        print($0)
                        return("failure 0")
                    }
                }
                i++
            }
            close(temp_file)
#            print("get_request.awk: failure in decoding output")
#            print("i is " i)
#            print(command)
#            print(temp_data)
#            return("failure 1")
            #"the kind that doesn't return anything"
            return(-1)
        } else {
            close(temp_file)
            print("get_request.awk: failure signal: " r)
            return("failure 2")
        }
#    }
}

function assertEqual(x, y){
    if(!(x == y)){
        print("failure. Assert equal failed. " x " " y)
        exit(-1)
    } else {
        return(0)
    }
}
