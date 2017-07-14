OTP_PLT=.otp_plt

BIN = ./bin
NOSE = $(BIN)/nosetests
PYTHON = $(BIN)/python
PIP = $(BIN)/pip

VER = 0.1.0
CORE = rel/ae_core/bin/ae_core
SWAGGER = apps/ae_http/src/swagger
SWTEMP := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir')
LOCAL = ./_build/local/rel

kill:
	@echo "Kill all beam processes only from this directory tree"
	$(shell pkill -9 -f ".*/beam.*-boot `pwd`" || true)

killall:
	@echo "Kill all beam processes from this host"
	@pkill -9 beam || true

dialyzer: $(OTP_PLT)
	@nice -19 \
		dialyzer apps/*/ebin/ --plts $(OTP_PLT) \
		-Wno_undefined_callbacks \
		-Wno_improper_lists

# Local

local-build: KIND=local
local-build: config/local/sys.config build

local-start: KIND=local
local-start: start

local-stop: KIND=local
local-stop: stop

local-attach: KIND=local
local-attach: attach

local-clean: KIND=local
local-clean: clean

# Production

prod-build: KIND=prod
prod-build: config/prod/sys.config build

prod-start: KIND=prod
prod-start: start

prod-stop: KIND=prod
prod-stop: stop

prod-attach: KIND=prod
prod-attach: attach

prod-clean: KIND=prod
prod-clean: clean

# Test

test-build: config/dev1/sys.config config/dev2/sys.config config/dev3/sys.config test1-build
	@rm -rf _build/dev2 _build/dev3
	@for x in dev2 dev3; do \
		cp -R _build/dev1 _build/$$x; \
		cp config/$$x/sys.config _build/$$x/rel/ae_core/releases/$(VER)/sys.config; \
		cp config/$$x/vm.args _build/$$x/rel/ae_core/releases/$(VER)/vm.args; \
		mkdir -p _build/$$x/rel/ae_core/keys; \
	done

test-start:
	@make test1-start
	@make test2-start
	@make test3-start

test-stop:
	@make test1-stop
	@make test2-stop
	@make test3-stop

test-clean:
	@make test1-clean
	@make test2-clean
	@make test3-clean

test1-build: KIND=dev1
test1-build: build

test1-start: KIND=dev1
test1-start: start

test1-stop: KIND=dev1
test1-stop: stop

test1-attach: KIND=dev1
test1-attach: attach

test1-clean: KIND=dev1
test1-clean: clean

test2-start: KIND=dev2
test2-start: start

test2-stop: KIND=dev2
test2-stop: stop

test2-attach: KIND=dev2
test2-attach: attach

test2-clean: KIND=dev2
test2-clean: clean

test3-start: KIND=dev3
test3-start: start

test3-stop: KIND=dev3
test3-stop: stop

test3-attach: KIND=dev3
test3-attach: attach

test3-clean: KIND=dev3
test3-clean: clean

#
# Build rules
#

.SECONDEXPANSION:

build: $$(KIND)
	@./rebar3 as $(KIND) release

start: $$(KIND)
	@./_build/$(KIND)/$(CORE) start

stop: $$(KIND)
	@./_build/$(KIND)/$(CORE) stop &

attach: $$(KIND)
	@./_build/$(KIND)/$(CORE) attach

clean: $$(KIND)
	@rm -rf ./_build/$(KIND)/rel/ae_core/data/*
	@rm -rf ./_build/$(KIND)/rel/ae_core/blocks/*
	@rm -rf ./config/$(KIND)/sys.config
	@rm -rf ./_build/$(KIND)/rel/*/log/*

$(LOCAL)/ae_core/keys:
	@mkdir -p $@

venv-present:
	@virtualenv -q .

prepare-nose-env: venv-present
	@. bin/activate && $(PIP) -q install -r requirements.txt

python-tests:
	@$(NOSE) --nocapture -c tests/nose.cfg

unit-tests:
	@./rebar3 do eunit,ct

swagger: config/swagger.yaml
	@swagger-codegen generate -i $< -l erlang-server -o $(SWTEMP)
	@echo "Swagger tempdir: $(SWTEMP)"
	@cp $(SWTEMP)/priv/swagger.json apps/ae_http/priv/
	@cp $(SWTEMP)/src/*.erl $(SWAGGER)/
	@rm -fr $(SWTEMP)

unlock:
	@./rebar3 unlock

lock:
	@./rebar3 lock

# 
# Deps
# 

config/local/sys.config: config/sys.config.tmpl
	sed -e "\
	s:%% comment:\
	{port, 3010},\
	{internal_port, 3011},\
	{keys_priv, <<\"laPlc2mJq5PM9AjIABaGHKpT/miiL0MNhm7puUh89JI=\">>},\
	{keys_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\
	{keys_pass, \"\"},\
	:\
	" $< > $@

config/prod/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 8040},\
    {internal_port, 8041},\
    {peers, [[{46,101,103,165}, 8080]]},\
    {master_pub, <<\"BO8I1h5yIliI8XPCT89TMTqWvsmZ0J0D13cwF8UZ9YrL2oIdKZUAVg2L100okp1wtYCecxPC8kyPigBMC/lvg1Y=\">>},\
    {test_mode,false},\
    :\
    " $< > $@

config/dev1/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 3010},\
    {internal_port, 3011},\
    {keys_priv, <<\"laPlc2mJq5PM9AjIABaGHKpT/miiL0MNhm7puUh89JI=\">>},\
    {keys_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\
    {keys_pass, \"\"},\
    :\
    " $< > $@

config/dev2/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 3020},\
    {internal_port, 3021},\
    {keys_pub, <<\"BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=\">>},\
    {keys_priv, <<\"GMwRk1KJtgJEH2RJp/XVeaQwJ4bpIqAr4lvQcIy4CSQ=\">>},\
    {keys_pass, \"\"},\
    :\
    " $< > $@

config/dev3/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 3030},\
    {internal_port, 3031},\
    {keys_pub, <<\"BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8=\">>},\
    {keys_priv, <<\"M/1xsM1DBO82qQcVJVoWVJd4p9YjpwygQJmmYkVLFd8=\">>},\
    {keys_pass, \"\"},\
    :\
    " $< > $@

tests: killall
	make test-build
	make test-clean
	make test-start
	@sleep 3
	make python-tests
	make test-stop

.PHONY: \
	local-build local-start local-stop local-attach local-clean \
	prod-build prod-start prod-stop prod-attach prod-clean \
	test-build  test-start test-stop test-clean \
	test1-start test1-stop test1-clean \
	test2-start test2-stop test2-clean \
	test3-start test3-stop test3-clean \
	dialyzer \
	venv-present \
	prepare-nose-env \
	python-tests \
	tests \
	unit-tests \
	unlock lock
