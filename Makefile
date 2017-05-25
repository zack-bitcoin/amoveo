OTP_PLT=.otp_plt

.PHONY: \
	compile \
	console \
	dialyzer \
	local-release \
	local-release-start \
	local-release-attach \
	local-release-clean \
	local-release-stop \
	prod-release \
	prod-release-start \
	prod-release-stop \
	dev-release \
	dev-release-start \
	dev-release-unikey \
	dev-release-stop \
	dev-release-clean \
	start-dev1 \
	start-dev2 \
	start-dev3 \
	stop-dev1 \
	stop-dev2 \
	stop-dev3 \
	attach-dev1 \
	attach-dev2 \
	attach-dev3 \
	clean-dev1 \
	clean-dev2 \
	clean-dev3 \
	release \
	release-start \
	release-stop \
	release-attach \
	tests


compile:
	@./rebar3 compile

console:
	@./rebar3 shell

dialyzer: $(OTP_PLT)
	@nice -19 \
		dialyzer apps/*/ebin/ --plts $(OTP_PLT) \
		-Wno_undefined_callbacks \
		-Wno_improper_lists

local-release:
	@./rebar3 as local release
	mkdir -p _build/local/rel/ae_core/keys/
	cp tests/masterkey/keys.db _build/local/rel/ae_core/keys/keys.db

local-release-start:
	@./_build/local/rel/ae_core/bin/ae_core start

local-release-attach:
	@./_build/local/rel/ae_core/bin/ae_core attach

local-release-clean:
	rm -rf ./_build/local/rel/ae_core/data/*
	rm -rf ./_build/local/rel/ae_core/blocks/*

local-release-stop:
	@./_build/local/rel/ae_core/bin/ae_core stop

prod-release:
	@./rebar3 as prod release

prod-release-start:
	@./_build/prod/rel/ae_core/bin/ae_core start

prod-release-stop:
	@./_build/prod/rel/ae_core/bin/ae_core stop

prod-release-attach:
	@./_build/prod/rel/ae_core/bin/ae_core attach

prod-release-clean:
	rm -rf ./_build/prod/rel/ae_core/data/*
	rm -rf ./_build/prod/rel/ae_core/blocks/*

dev-masterkey:
	mkdir -p _build/dev1/rel/ae_core/keys/ _build/dev2/rel/ae_core/keys/ _build/dev3/rel/ae_core/keys/
	cp tests/masterkey/keys.db _build/dev1/rel/ae_core/keys/keys.db
	cp tests/masterkey/keys.db _build/dev2/rel/ae_core/keys/keys.db
	cp tests/masterkey/keys.db _build/dev3/rel/ae_core/keys/keys.db

# Copy dev1 node instead re-compiling same core apps to increase build speed
# TODO: parametrize release version to make it flexible
dev-release:
	rm -rf _build/dev1
	@./rebar3 as dev1 release
	#@./rebar3 as dev2 release
	#@./rebar3 as dev3 release
	rm -rf _build/dev2
	cp -R _build/dev1 _build/dev2
	rm -rf _build/dev3
	cp -R _build/dev1 _build/dev3
	cp config/dev2/sys.config _build/dev2/rel/ae_core/releases/0.1.0/sys.config
	cp config/dev2/vm.args _build/dev2/rel/ae_core/releases/0.1.0/vm.args
	cp config/dev3/sys.config _build/dev3/rel/ae_core/releases/0.1.0/sys.config
	cp config/dev3/vm.args _build/dev3/rel/ae_core/releases/0.1.0/vm.args

dev-release-unikey: dev-release dev-masterkey

dev-release-start:
	@./_build/dev1/rel/ae_core/bin/ae_core start
	@./_build/dev2/rel/ae_core/bin/ae_core start
	@./_build/dev3/rel/ae_core/bin/ae_core start

dev-release-stop:
	@./_build/dev1/rel/ae_core/bin/ae_core stop
	@./_build/dev2/rel/ae_core/bin/ae_core stop
	@./_build/dev3/rel/ae_core/bin/ae_core stop

dev-release-clean: clean-dev1 clean-dev2 clean-dev3

start-dev1:
	@./_build/dev1/rel/ae_core/bin/ae_core start

start-dev2:
	@./_build/dev2/rel/ae_core/bin/ae_core start

start-dev3:
	@./_build/dev3/rel/ae_core/bin/ae_core start

stop-dev1:
	@./_build/dev1/rel/ae_core/bin/ae_core stop

stop-dev2:
	@./_build/dev2/rel/ae_core/bin/ae_core stop

stop-dev3:
	@./_build/dev3/rel/ae_core/bin/ae_core stop

attach-dev1:
	@./_build/dev1/rel/ae_core/bin/ae_core attach

attach-dev2:
	@./_build/dev2/rel/ae_core/bin/ae_core attach

attach-dev3:
	@./_build/dev3/rel/ae_core/bin/ae_core attach

clean-dev1:
	rm -rf ./_build/dev1/rel/ae_core/data/*
	rm -rf ./_build/dev1/rel/ae_core/blocks/*

clean-dev2:
	rm -rf ./_build/dev2/rel/ae_core/data/*
	rm -rf ./_build/dev2/rel/ae_core/blocks/*

clean-dev3:
	rm -rf ./_build/dev3/rel/ae_core/data/*
	rm -rf ./_build/dev3/rel/ae_core/blocks/*

release: local-release

release-start: local-release-start

release-stop: local-release-stop

release-attach: local-release-attach

tests:
	@./rebar3 do eunit,ct
