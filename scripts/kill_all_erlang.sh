for i in `ps -ef | grep erl | awk '{print $2}'`; do echo $i; kill -9 $i; done
