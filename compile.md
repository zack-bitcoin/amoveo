For ubuntu, I needed to install these dependencies:

```
sudo apt-get install erlang libncurses5-dev libssl-dev unixodbc-dev g++ git erlang-base-hipe
```

Next, download Aeternity Testnet.

```
git clone https://github.com/Aeternity/Testnet.git
```
Now you can go into the directory, and compile the aeternity testnet.

```
cd testnet/
sh install.sh
```
Start your node with this script:

```
sh start.sh
```
Then open this URL in your browser: http://localhost:3011/login.html

When you are done, you can turn the node off by clicking "halt" in your browser, or by running this command in the flying fox command line interface:

```
testnet_sup:stop()
```