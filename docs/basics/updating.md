It should take less than 20 seconds to update your node.

First, if your node is on, turn it off.
```
api:off().
halt().
```

Then get the update
```
git pull
```

Finally, turn it back on
```
make prod-restart
```