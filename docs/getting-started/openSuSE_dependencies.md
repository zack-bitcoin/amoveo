Installation of Aeternity on OpenSuSE Leap 42.3
... requires Erlang 

OpenSuSE Leap 42.3
==================
When doing a fresh OS install, choose "minimal server install".
Once on the running system, check that these packages get/are installed:
```
+ X Windows (e.g. xorg, xdm and icewm or chose KDE or GNOME)
+ java-1_8_0-openjdk-Devel 
+ gcc
+ gcc-c++
+ git
+ make
+ m4
+ ncurses-devel
+ libopenssl-devel
```

Erlang
======
get the source:
```
wget http://erlang.org/download/otp_src_20.0.tar.gz
```
verify the tarball:
```
tar tvzf otp_src_20.0.tar.gz
```
install tarball:
```
tar xzf otp_src_20.0.tar.gz
```
prepare environment (follow steps of erlang install readme):
```
cd otp_src_20.0
export ERL_TOP=`pwd`
export LANG=C
./configure --enable-hipe
```
and compile & test it (still along erlang docs):
```
make
make release_tests
cd release/tests/test_server
$ERL_TOP/bin/erl -s ts install -s ts smoke_test batch -s init stop
```
verify results in a browser:
```
lynx index.html 
```
--> there might be some testcases which are skipped... it's ok.

and install erlang:
```
cd $ERL_TOP
sudo make install 
```
start erlang from your command line and see, if [hipe] is displayed.


Aeternity
=========
change to your home directory, get the source code, and compile it:
```
git clone https://github.com/aeternity/testnet.git
cd testnet/
```
follow the steps of the install docs, basically:
```
  make prod-build
  make prod-go
  make attach
```



