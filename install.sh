# first install rebar package manager
if [ -e "rebar" ]
then
    echo "rebar already installed"
elif [ "$(uname -s)" == "Linux" ]; then
    wget https://raw.githubusercontent.com/wiki/rebar/rebar/rebar && chmod u+x rebar
    ./rebar get
    ./rebar compile
    echo "Successfully compiled Aeternity testnet".
elif [ "$(uname -s)" == "Darwin" ]; then
    curl https://raw.githubusercontent.com/wiki/rebar/rebar/rebar -o rebar
    chmod u+x rebar
    ./rebar get
    ./rebar compile
    echo "Successfully compiled Aeternity testnet".
else
    echo "your computer cannot compile this"
fi

#use rebar to install other dependencies, explained in rebar.config

sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.


