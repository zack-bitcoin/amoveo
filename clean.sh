rm data/*.db
rm blocks/*.db

if [ -d "backup" ]
then
    touch backup/temp.db
    rm backup/*.db
else
    mkdir backup
fi

if [ -e "data/keys_backup" ]
then
    cp data/keys_backup data/keys.db
fi
