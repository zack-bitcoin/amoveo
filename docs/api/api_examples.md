Example of how to add a node to the list of nodes you share blocks with. This is an example of accessing the local api on the same machine.

```
curl -i -d '["add_peer", [127,0,0,1], 3011]' http://localhost:8041
```

#example response

```
HTTP/1.1 200 OK
server: Cowboy
date: Fri, 14 Apr 2017 09:49:41 GMT
content-length: 4
content-type: application/octet-stream
Access-Control-Allow-Origin: *

"ok"
```

example of executing this api request from javascript
first make sure that rpc.js is loaded, then you can do this:

```
local_get(["add_peer", [127,0,0,1], 3011]);
```

[The internal API is defined here](../src/networking/internal_handler.erl)

Now an example of accessing an api of a different node.
This is how you request the header of the genesis block.
Notice that the ip for external api is one lower than the ip for the api that is only on the same node.

```
curl -i -d '["header", 0]' http://localhost:8040
```

example response

```
HTTP/1.1 200 OK
server: Cowboy
date: Fri, 14 Apr 2017 09:56:29 GMT
content-length: 53
content-type: application/octet-stream
Access-Control-Allow-Origin: *

["ok","AAAAAAAAAAAAAAAA1oYN5PPka/zJxej5AAAAAAAP8AAB"]
```

Here is an example of accessing the genesis block from javascript. you need to load rpc.js before you can do this.

```
function callback(x) {
	 console.log("the header is ");
	 console.log(x);
};
variable_public_get(["header", 0], callback);
```

[The external API is defined here](../../apps/amoveo_http/src/ext_handler.erl)

[The internal API is defined here](../../apps/amoveo_http/src/api.erl)
