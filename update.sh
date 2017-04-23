git pull origin master
if [ `uname -s`==Linux ]
then
./rebar get-deps
./rebar compile 
elif [ `uname -s`==Darwin ]
then
rebar get-deps
rebar compile 
else
    echo "your computer cannot update this"
fi
