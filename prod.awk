BEGIN {
    #- @curl -i -d '["off"]' http://127.0.0.1:8081
    system("./scripts/config_setup.sh")
    #config/prod/sys.config
    #config/sys.config.tmpl
    system("./rebar3 as prod release")
    print("is compiling")
    system("mkdir -p ./_build/prod/rel/amoveo_core/ebin")
    system("mkdir -p ./_build/prod/rel/amoveo_core/precomputes")
    system("gcc -O2 -march=native -funroll-loops -fomit-frame-pointer -flto -fPIC -shared -o ./_build/prod/rel/amoveo_core/ebin/fr.so ./_build/prod/lib/verkle/src/crypto/fr.c -I $ERL_ROOT/user/include/ -I /usr/lib/erlang/erts-16.3/include ")
    system("gcc -O2 -march=native -funroll-loops -fomit-frame-pointer -flto -fPIC -shared -o ./_build/prod/rel/amoveo_core/ebin/ed25519.so ./_build/prod/lib/verkle/src/crypto/ed25519.c -I $ERL_ROOT/user/include/ -I /usr/lib/erlang/erts-16.3/include ")

    config_string = "{port, 8080},\n {kind, \"production\"},\n {internal_port, 8081},\n {swagger_port_internal, 8042},\n {swagger_port_external, 8043},\n {peers, [{{193,178,170,111},8080},{{159,223,85,216},8080},{{159,65,126,146},8080},{{64,227,21,70},8080},{{46,101,81,5},8080},{{43,133,42,108},8080}]},\n {pools, [{{193,178,170,111},8080},{{159,223,85,216},8080},{{159,65,126,146},8080},{{43,133,42,108},8080}]},\n {master_pub, <<\"BL0SzhkFGFW1kTTdnO8sGnwPEzUvx2U2nyECwWmUJPRhLxbPPK+ep8eYMxlTxVO/wnQS5WmsGIKcrPP7/Fw1WVc=\">>},\n {test_mode,false},\n {request_frequency, 10},\n"

    system("rm config/prod/sys.config")

    while((getline line < "config/sys.config.tmpl") > 0){
	if(line ~ /comment/){
	    print config_string >> "config/prod/sys.config"
	} else {
	    print line >> "config/prod/sys.config"
	}
    }
    system("sleep 0.2")
   
    system("./_build/prod/rel/amoveo_core/bin/amoveo_core daemon")
    
    #make prod-go
    #sleep 3
    #make prod-attach

}
