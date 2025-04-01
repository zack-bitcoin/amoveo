
   {
       #if(match($0, /\[0-9a-zA-Z,{}"'=\/\-\[\]+ ]*/)){
       if(match($0, /\["ok",[a-zA-Z0-9,{}"'=\/\[\]+ \-_]*\]$/)){
           #print($0)
           print(RSTART " " RLENGTH)
           print(substr($0, RSTART, RLENGTH))
           print("matched")
       } else {
           print($0)
           print("not matched")
       }
   }
   
           
