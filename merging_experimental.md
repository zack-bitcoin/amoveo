1) comment out `sh clean.sh` and `git pull origin experimental` from start.sh
2) don't assume the password is "abc" in start.sh
3) in start.sh, automatically add some peers. {46,101,103,165}, 8080
3) increase difficulty
4) don't automatically pull in start.sh
5) channel_solo_close check slash timer needs to sleep long enough.