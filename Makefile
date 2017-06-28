OTP_PLT=.otp_plt

.PHONY: \
	release-build \
	release-start \
	release-attach \
	release-clean \
	release-end \
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
	@./rebar3 as local release
	mkdir -p _build/local/rel/ae_core/keys/
	cp tests/masterkey/keys.db _build/local/rel/ae_core/keys/keys.db

test-release-start:
	@./_build/local/rel/ae_core/bin/ae_core start

test-release-attach:
	@./_build/local/rel/ae_core/bin/ae_core attach

test-release-end:
	@./_build/local/rel/ae_core/bin/ae_core stop

test-release-clean:
	rm -rf ./_build/local/rel/ae_core/data/*
	rm -rf ./_build/local/rel/ae_core/blocks/*

release-build:
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
	cp tests/masterkey/keys.db _build/dev1/rel/ae_core/keys/keys.db

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
