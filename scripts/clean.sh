if [ -e "data/keys.db" ]
then
    cp data/keys.db keys_backup
fi
rm -r data
mkdir data
rm -r blocks
mkdir blocks

if [ -d "backup" ]
then
    touch backup/temp.db
    rm backup/*.db
else
    mkdir backup
fi

#if [ -e "data/keys_backup" ]
#then
cp keys_backup data/keys.db
#fi
