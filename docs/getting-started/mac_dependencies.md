install erlang version 18 or higher.
You may need to install from source http://www.erlang.org/downloads

Or use [Homebrew](https://brew.sh):
```
brew install erlang
```

It depends on sed, the stream editor
```
brew install gnu-sed --with-default-names
```

You need pip in order to install the next step:
```
easy_install pip
```

You will need virtualenv for making a version of python with custom libraries for running tests.
```
pip install --user virtualenv
```

Use git to download the software, then go into the testnet directory
```
git clone https://github.com/zack-bitcoin/amoveo.git
cd amoveo
```
