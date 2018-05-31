OTP_PLT=.otp_plt

BIN = ./bin
NOSE = $(BIN)/nosetests
PYTHON = $(BIN)/python
PIP = $(BIN)/pip

VER = 0.1.0
CORE = rel/amoveo_core/bin/amoveo_core
SWAGGER = apps/amoveo_http/src/swagger
SWTEMP := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir')
LOCAL = ./_build/local/rel

kill:
	@echo "Kill all beam processes only from this directory tree"
	$(shell pkill -9 -f ".*/beam.*-boot `pwd`" || true)

killall:
	@echo "Kill all beam processes from this host"
	@pkill -9 beam || true

dialyzer:
	@./rebar3 dialyzer


# Local

local-build: KIND=local
local-build: config/local/sys.config build

local-go: KIND=local
local-go: go

local-stop: KIND=local
local-stop: stop

local-attach: KIND=local
local-attach: attach

local-clean: KIND=local
local-clean: clean

# Production

prod-build: KIND=prod
prod-build: config/prod/sys.config build

prod-go: KIND=prod
prod-go: go

prod-stop: KIND=prod
prod-stop: stop

prod-attach: KIND=prod
prod-attach: attach

prod-clean: KIND=prod
prod-clean: clean

prod-blocks: KIND=prod
prod-blocks: blocks

# Test

multi-build: config/dev1/sys.config config/dev2/sys.config config/dev3/sys.config build1
	@rm -rf _build/dev2 _build/dev3
	@for x in dev2 dev3; do \
		cp -R _build/dev1 _build/$$x; \
		cp config/$$x/sys.config _build/$$x/rel/amoveo_core/releases/$(VER)/sys.config; \
		cp config/$$x/vm.args _build/$$x/rel/amoveo_core/releases/$(VER)/vm.args; \
		mkdir -p _build/$$x/rel/amoveo_core/keys; \
	done

multi-go:
	@make go1
	@make go2
	@make go3

multi-stop:
	@make stop1 &
	@make stop2 &
	@make stop3

multi-clean:
	@make clean1
	@make clean2
	@make clean3

build1: KIND=dev1
build1: build

go1: KIND=dev1
go1: go

stop1: KIND=dev1
stop1: stop

attach1: KIND=dev1
attach1: attach

clean1: KIND=dev1
clean1: clean

go2: KIND=dev2
go2: go

stop2: KIND=dev2
stop2: stop

attach2: KIND=dev2
attach2: attach

clean2: KIND=dev2
clean2: clean

go3: KIND=dev3
go3: go

stop3: KIND=dev3
stop3: stop

attach3: KIND=dev3
attach3: attach

clean3: KIND=dev3
clean3: clean

#
# Build rules
#

.SECONDEXPANSION:

build: $$(KIND)
	@./rebar3 as $(KIND) release

go: $$(KIND)
	@./_build/$(KIND)/$(CORE) start

stop: $$(KIND)
	@./_build/$(KIND)/$(CORE) stop &

attach: $$(KIND)
	@./_build/$(KIND)/$(CORE) attach

clean: $$(KIND)
	@rm -rf ./_build/$(KIND)/rel/amoveo_core/data/
	@mkdir ./_build/$(KIND)/rel/amoveo_core/data
	@rm -rf ./_build/$(KIND)/rel/amoveo_core/blocks/
	@mkdir ./_build/$(KIND)/rel/amoveo_core/blocks/
	@touch ./config/$(KIND)/sys.config
	@rm  ./config/$(KIND)/sys.config
	@rm -rf ./_build/$(KIND)/rel/log/
	@mkdir ./_build/$(KIND)/rel/log

$(LOCAL)/amoveo_core/keys:
	@mkdir -p $@

#venv-present:
#	@virtualenv -q .

#nose-env: venv-present
#	@. bin/activate && $(PIP) -q install -r requirements.txt

#python-tests:
#	@$(NOSE) --nocapture -c tests/nose.cfg

unit-tests:
	@./rebar3 do eunit,ct

swagger: config/swagger.yaml
	@swagger-codegen generate -i $< -l erlang-server -o $(SWTEMP)
	@echo "Swagger tempdir: $(SWTEMP)"
	@cp $(SWTEMP)/priv/swagger.json apps/amoveo_http/priv/
	@cp $(SWTEMP)/src/*.erl $(SWAGGER)/
	@rm -fr $(SWTEMP)

#for rebar.lock
dependency-unlock:
	@./rebar3 unlock

dependency-lock:
	@./rebar3 lock

config/local/sys.config: config/sys.config.tmpl
	sed -e "\
	s:%% comment:\
	{port, 3010},\
	{kind, \"local\"},\
	{internal_port, 3011},\
	{swagger_port_internal, 3012},\
	{swagger_port_external, 3013},\
	{keys_priv, <<\"laPlc2mJq5PM9AjIABaGHKpT/miiL0MNhm7puUh89JI=\">>},\
	{keys_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\
	{keys_pass, \"\"},\
        {request_frequency, 100},\
	:\
	" $< > $@

config/prod/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 8080},\
    {kind, \"production\"},\
    {internal_port, 8081},\
    {swagger_port_internal, 8042},\
    {swagger_port_external, 8043},\
    {peers, [{{88,198,41,199}, 8080},{{78,46,149,239}, 8080},{{73,95,148,9}, 8080},{{70,133,222,59}, 8080},{{69,137,111,9}, 8080},{{51,15,87,84}, 8080},{{51,15,75,100}, 8080},{{51,15,69,135}, 8080},{{5,134,115,23}, 8080},{{47,75,188,95}, 8080},{{45,77,8,119}, 8080},{{35,229,47,138}, 8080},{{35,229,145,49}, 8080},{{213,231,131,153}, 8080},{{209,182,233,16}, 8080},{{207,154,237,109}, 8080},{{204,48,31,181}, 8080},{{178,62,56,56}, 8080},{{176,9,84,75}, 8080},{{176,9,4,144}, 8080},{{173,230,157,155}, 8080},{{168,62,52,179}, 8080},{{159,89,106,253}, 8080},{{159,65,193,162}, 8080},{{159,65,173,9}, 8080},{{159,65,120,84}, 8080},{{138,197,189,84}, 8080}]},\
     {pools, [{{159,65,120,84}, 8080},{{173,230,157,155}, 8080},{{51,15,87,84}, 8080},{{159,65,173,9},8080}, {{47,75,188,95}, 8080},{{47,105,59,4}, 8080},{{47,105,43,173}, 8080},{{114,215,136,52}, 8080},{{176,9,76,201}, 8080},{{47,75,91,194}, 8080}]},\
    {master_pub, <<\"BL0SzhkFGFW1kTTdnO8sGnwPEzUvx2U2nyECwWmUJPRhLxbPPK+ep8eYMxlTxVO/wnQS5WmsGIKcrPP7/Fw1WVc=\">>},\
    {test_mode,false},\
    {request_frequency, 10},\
    :\
    " $< > $@
   #{peers, []},\

config/dev1/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 3010},\
    {kind, \"integration\"},\
    {internal_port, 3011},\
    {swagger_port_internal, 3012},\
    {swagger_port_external, 3013},\
    {keys_priv, <<\"laPlc2mJq5PM9AjIABaGHKpT/miiL0MNhm7puUh89JI=\">>},\
    {keys_pub, <<\"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=\">>},\
    {keys_pass, \"\"},\
    {request_frequency, 100},\
    :\
    " $< > $@

config/dev2/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 3020},\
    {kind, \"integration\"},\
    {internal_port, 3021},\
    {swagger_port_internal, 3022},\
    {swagger_port_external, 3023},\
    {keys_pub, <<\"BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=\">>},\
    {keys_priv, <<\"GMwRk1KJtgJEH2RJp/XVeaQwJ4bpIqAr4lvQcIy4CSQ=\">>},\
    {keys_pass, \"\"},\
    {request_frequency, 100},\
    :\
    " $< > $@

config/dev3/sys.config: config/sys.config.tmpl
	sed -e "\
    s:%% comment:\
    {port, 3030},\
    {kind, \"integration\"},\
    {internal_port, 3031},\
    {swagger_port_internal, 3032},\
    {swagger_port_external, 3033},\
    {keys_pub, <<\"BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8=\">>},\
    {keys_priv, <<\"M/1xsM1DBO82qQcVJVoWVJd4p9YjpwygQJmmYkVLFd8=\">>},\
    {keys_pass, \"\"},\
    {request_frequency, 100},\
    :\
    " $< > $@

tests: killall
	make multi-build
	make multi-clean
	make multi-go
	@sleep 3
	@python tests/test_all.py
	make multi-stop

multi-quick: kill multi-build multi-clean multi-go

local-quick: kill local-build local-clean
	./_build/local/rel/amoveo_core/bin/amoveo_core console
prod-restart: prod-stop prod-build prod-go
	@sleep 3
	@make prod-attach

.PHONY: \
	local-build local-go local-stop local-attach local-clean \
	prod-build prod-go prod-stop prod-attach prod-clean \
	multi-build  multi-go multi-stop multi-clean \
	go1 stop1 clean1 attach1 \
	go2 stop2 clean2 attach2 \
	go3 stop3 clean3 attach3 \
	dialyzer \
	venv-present \
	nose-env \
	python-tests \
	tests \
	unit-tests \
	dependency-unlock dependency-lock
