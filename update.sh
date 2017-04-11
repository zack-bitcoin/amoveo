git pull origin master
if [ `uname -s`==Linux ]
then
./rebar get-deps
./rebar get
./rebar compile 
elif [ `uname -s`==Darwin ]
then
rebar get-deps
rebar get
rebar compile 
else
    echo "your computer cannot update this"
fi
