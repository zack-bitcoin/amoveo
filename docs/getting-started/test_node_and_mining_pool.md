
## Launching Amoveo Test Node and Integrating Mining Pool.

#### During the process, if you face any problems, please see troubleshooting section at the end of this doc.

##### 1/ Cloning source
https://github.com/zack-bitcoin/amoveo

https://github.com/zack-bitcoin/amoveo-mining-pool

Remember to install dependencies:

https://github.com/zack-bitcoin/amoveo/blob/master/docs/getting-started/dependencies.md

##### 2/ Compiling
###### a. Compiling node
cd amoveo
make local-build
To run in console mode:
```
./_build/local/rel/amoveo_core/bin/amoveo_core console
```
To run in background mode:
```
./_build/local/rel/amoveo_core/bin/amoveo_core start
```

###### b. Compiling mining pool source
```
cd amoveo-mining-pool
vi ./apps/amoveo_mining_pool/src/config.erl
```
Comment out mode() -> production by adding `%` before it
Turn on mode()->test by removing `%` before it
```
./rebar3 compile
./rebar3 as prod release
```

Starting the pool:
```
./_build/prod/rel/amoveo_mining_pool/bin/amoveo_mining_pool console
```
background mode:
```
./_build/prod/rel/amoveo_mining_pool/bin/amoveo_mining_pool start
```
##### 3/ Now test your pool
run this in terminal
```
curl -i -d '["mining_data"]' http://127.0.0.1:8085
```
If it shows something like:
```
["ok",[-6,"F8iaECO9d0M2kmik0g0i9nMfAf6URiPOcrA0+VMIEUs=","52MYc3JWoVbIfHV0VEYTWqH/uf+R+qk=",257]]
```
Then you’re done!

### Troubleshooting
test node doesn’t fetch or does anything with real data, so it’s very easy to debug. Just run console mode to see logs. 
##### The usual issue is erlang node keeps running in background after crashing. To kill it:
```
ps aux | grep "veo"
```
Remember the pid of veo process then:
```
kill [pid]
```
more brutal:
```
kill -9 [pid]
```
Then run stuff again in console mode

##### If you mess up any source code and have no idea to fix:
```
git commit -am "mess up stuff"
rm -rf _build
git reset --hard origin/master
```
You can use git reflog and checkout to see what you’ve modified.
