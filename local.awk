BEGIN {
#kill
#scripts/config_setup.sh
#local-build
#local-clean
#local-compile
    #./_build/local/rel/amoveo_core/bin/amoveo_core console


    system("./scripts/config_setup.sh")
    system("./rebar3 as local release")
    print("is compiling")
    system("mkdir -p ./_build/local/rel/amoveo_core/ebin")
    system("mkdir -p ./_build/local/rel/amoveo_core/precomputes")
    system("gcc -O2 -march=native -funroll-loops -fomit-frame-pointer -flto -fPIC -shared -o ./_build/local/rel/amoveo_core/ebin/fr.so ./_build/local/lib/verkle/src/crypto/fr.c -I $ERL_ROOT/user/include/ -I /usr/lib/erlang/erts-16.3/include ")
    system("gcc -O2 -march=native -funroll-loops -fomit-frame-pointer -flto -fPIC -shared -o ./_build/local/rel/amoveo_core/ebin/ed25519.so ./_build/local/lib/verkle/src/crypto/ed25519.c -I $ERL_ROOT/user/include/ -I /usr/lib/erlang/erts-16.3/include ")

    config_string = "%% comment:\n {port, 3010},\n {kind, \"local\"},\n {internal_port, 3011},\n {swagger_port_internal, 3012},\n {swagger_port_external, 3013},\n {keys_priv, <<\"laPlc2mJq5PM9AjIABaGHKpT/miiL0MNhm7puUh89JI=\">>},\n {keys_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\n {keys_pass, \"\"},\n {request_frequency, 100},\n {master_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\n {test_mode, true},\n"

    system("rm config/local/sys.config")

    while((getline line < "config/sys.config.tmpl") > 0){
	if(line ~ /comment/){
	    print config_string >> "config/local/sys.config"
	} else {
	    print line >> "config/local/sys.config"
	}
    }
    system("sleep 0.2")

    system("./_build/local/rel/amoveo_core/bin/amoveo_core console")
    
}
