sed '1,1d' ~/.erlang.cookie
openssl rand -base64 48 >> ~/.erlang.cookie
