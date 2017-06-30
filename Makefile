OTP_PLT=.otp_plt

.PHONY: \
	release-build \
	release-start \
	release-attach \
	release-end \
	release-clean \
	test-release-build \
	test-release-start \
	test-release-attach \
	test-release-end \
	test-release-clean \
	multi-test-release-build \
	multi-test-release-start \
	multi-test-release-end \
	multi-test-release-clean \
	start-1 \
	start-2 \
	start-3 \
	attach-1 \
	attach-2 \
	attach-3 \
	end-1 \
	end-2 \
	end-3 \
	clean-1 \
	clean-2 \
	clean-3 \
	compile \
	console \
	dialyzer \
	unit-tests

#compile:
#	@./rebar3 compile

#console:
#	@./rebar3 shell

dialyzer: $(OTP_PLT)
	@nice -19 \
		dialyzer apps/*/ebin/ --plts $(OTP_PLT) \
		-Wno_undefined_callbacks \
		-Wno_improper_lists

test-release-build:
	sed -e "\
	s:%% comment:\
	{port, 3010},\
	{keys_priv, <<\"laPlc2mJq5PM9AjIABaGHKpT/miiL0MNhm7puUh89JI=\">>},\
	{keys_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\
	{keys_pass, \"\"},\
	{keys_id, 1},\
	:\
	" config/sys.config > config/local/sys.config

	@./rebar3 as local release
	mkdir -p _build/local/rel/ae_core/keys/

test-release-start:
	@./_build/local/rel/ae_core/bin/ae_core start

test-release-attach:
	@./_build/local/rel/ae_core/bin/ae_core attach

test-release-end:
	@./_build/local/rel/ae_core/bin/ae_core stop &

test-release-clean:
	rm -rf ./_build/local/rel/ae_core/data/*
	rm -rf ./_build/local/rel/ae_core/blocks/*

release-build:
	sed -e "\
    s:%% comment:\
    {port, 8040},\
    {peers, [[{46,101,103,165}, 8080]]},\
    {master_pub, <<\"BO8I1h5yIliI8XPCT89TMTqWvsmZ0J0D13cwF8UZ9YrL2oIdKZUAVg2L100okp1wtYCecxPC8kyPigBMC/lvg1Y=\">>},\
    {test_mode,false},\
    :\
    " config/sys.config > config/prod/sys.config
	@./rebar3 as prod release

release-start:
	@./_build/prod/rel/ae_core/bin/ae_core start

release-attach:
	@./_build/prod/rel/ae_core/bin/ae_core attach

release-end:
	@./_build/prod/rel/ae_core/bin/ae_core stop

release-clean:
	rm -rf ./_build/prod/rel/ae_core/data/*
	rm -rf ./_build/prod/rel/ae_core/blocks/*

# TODO: parametrize release version to make it flexible
multi-test-release-build:
	sed -e "\
    s:%% comment:\
    {port, 3010},\
    {keys_priv, <<\"laPlc2mJq5PM9AjIABaGHKpT/miiL0MNhm7puUh89JI=\">>},\
    {keys_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\
    {keys_pass, \"\"},\
    {keys_id, 1},\
    :\
    " config/sys.config > config/dev1/sys.config
	sed -e "\
    s:%% comment:\
    {port, 3020},\
    {keys_pub, <<\"BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=\">>},\
    {keys_priv, <<\"GMwRk1KJtgJEH2RJp/XVeaQwJ4bpIqAr4lvQcIy4CSQ=\">>},\
    {keys_pass, \"\"},\
    {keys_id, 2},\
    :\
    " config/sys.config > config/dev2/sys.config
	sed -e "\
    s:%% comment:\
    {port, 3030},\
    {keys_pub, <<\"BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8=\">>},\
    {keys_priv, <<\"M/1xsM1DBO82qQcVJVoWVJd4p9YjpwygQJmmYkVLFd8=\">>},\
    {keys_pass, \"\"},\
    {keys_id, 3},\
    :\
    " config/sys.config > config/dev3/sys.config

	rm -rf _build/dev1
	@./rebar3 as dev1 release
	rm -rf _build/dev2
	cp -R _build/dev1 _build/dev2
	rm -rf _build/dev3
	cp -R _build/dev1 _build/dev3
	cp config/dev2/sys.config _build/dev2/rel/ae_core/releases/0.1.0/sys.config
	cp config/dev2/vm.args _build/dev2/rel/ae_core/releases/0.1.0/vm.args
	cp config/dev3/sys.config _build/dev3/rel/ae_core/releases/0.1.0/sys.config
	cp config/dev3/vm.args _build/dev3/rel/ae_core/releases/0.1.0/vm.args
	mkdir -p _build/dev1/rel/ae_core/keys/ _build/dev2/rel/ae_core/keys/ _build/dev3/rel/ae_core/keys/

multi-test-release-start: start-1 start-2 start-3

multi-test-release-end: end-1 end-2 end-3

multi-test-release-clean: clean-1 clean-2 clean-3

start-1:
	@./_build/dev1/rel/ae_core/bin/ae_core start

start-2:
	@./_build/dev2/rel/ae_core/bin/ae_core start

start-3:
	@./_build/dev3/rel/ae_core/bin/ae_core start

attach-1:
	@./_build/dev1/rel/ae_core/bin/ae_core attach

attach-2:
	@./_build/dev2/rel/ae_core/bin/ae_core attach

attach-3:
	@./_build/dev3/rel/ae_core/bin/ae_core attach

end-1:
	@./_build/dev1/rel/ae_core/bin/ae_core stop &

end-2:
	@./_build/dev2/rel/ae_core/bin/ae_core stop &

end-3:
	@./_build/dev3/rel/ae_core/bin/ae_core stop &

clean-1:
	rm -rf ./_build/dev1/rel/ae_core/data/*
	rm -rf ./_build/dev1/rel/ae_core/blocks/*

clean-2:
	rm -rf ./_build/dev2/rel/ae_core/data/*
	rm -rf ./_build/dev2/rel/ae_core/blocks/*

clean-3:
	rm -rf ./_build/dev3/rel/ae_core/data/*
	rm -rf ./_build/dev3/rel/ae_core/blocks/*

unit-tests:
	@./rebar3 do eunit,ct
