cd ..
sudo rm -r testnet2
sudo rm -r testnet3
cp -r zack_testnet testnet2
cp -r zack_testnet testnet3
#rm -r testnet2/ebin
#rm -r testnet3/ebin
rm testnet2/data/keys*
rm testnet3/data/keys*
