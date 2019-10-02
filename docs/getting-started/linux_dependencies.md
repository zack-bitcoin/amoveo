First you have to install the dependencies. This depends on your Linux distribution. If unsure you can try `lsb_release -d` to check your distribution.

Then install dependencies using the instructions for your distribution:
- [Ubuntu](#for-ubuntu)
- [ArchLinux](#for-archlinux)
- [OpenSuSE](#for-opensuse)

Finally proceed to [instructions after dependencies](#after-dependencies)

## For Ubuntu

1. Make sure that that you are running Ubuntu 18 or later:

    (earlier versions of ubuntu requires manually installing the latest version of erlang, because the package manager installs an old version)

    Use this command to check your version number
    ```
    lsb_release -a
    ```

2. Make sure that your system is up-to-date:
   ```
   sudo apt-get update
   ```
   and
   ```
   sudo apt-get upgrade
   ```

3. Install following dependencies:
   ```
   sudo apt install erlang-asn1 erlang-public-key erlang-ssl erlang-inets erlang-jiffy erlang-dev erlang-base-hipe libncurses5-dev libssl-dev unixodbc-dev g++ git make python
   ```

## For ArchLinux

1. Make sure your system is up to date. This step is important, because it also synchronises repository database:
    ```
    sudo pacman -Syu
    ```

2. Install the dependencies:
    ```
    pacman -S --needed community/erlang extra/git extra/unixodbc core/gcc core/ncurses extra/wget
    ```
    you also need `awk` and `make`

## For OpenSuSE

Tested on OpenSuSE Leap 42.3.
OpenSuSE requires manual compilation and instalation of Erlang.

1. Install the dependencies:

```
+ java-1_8_0-openjdk-Devel 
+ gcc
+ gcc-c++
+ git
+ make
+ m4
+ ncurses-devel
+ libopenssl-devel
```

2. Install Erlang from source:

- get the source:
  ```
  wget http://erlang.org/download/otp_src_20.0.tar.gz
  ```

- verify the tarball:
  ```
  tar tvzf otp_src_20.0.tar.gz
  ```

- unpack tarball:
  ```
  tar xzf otp_src_20.0.tar.gz
  ```

- prepare environment (follow steps of erlang install readme):
  ```
  cd otp_src_20.0
  export ERL_TOP=`pwd`
  export LANG=C
  ./configure --enable-hipe
  ```

- compile & test it (still along erlang docs):
  ```
  make
  make release_tests
  cd release/tests/test_server
  $ERL_TOP/bin/erl -s ts install -s ts smoke_test batch -s init stop
  ```

- verify results in a browser (for example lynx):
  ```
  lynx index.html 
  ```

- install erlang:
  ```
  cd $ERL_TOP
  sudo make install 
  ```
- start erlang from your command line and see, if [hipe] is displayed.


## After Dependencies

Next, download Amoveo. You should run following steps with a non-root user, for better security.

```
git clone https://github.com/zack-bitcoin/amoveo.git
```
Now you can go into the directory, and compile Amoveo.

```
cd amoveo/
make prod-build
```

Now you can run your node. For instructions proceed to [turn_it_on](turn_it_on.md).
