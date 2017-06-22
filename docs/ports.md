hmmmm any ports i need to make sure are open?

[8:53]
8040 / 3010 / 3020?

zack
[8:54 PM]
you can start it on whatever port you want

[8:54]
`sh start.sh 12121` for example

howard [8:54 PM]
what is the default port?

zack [8:54 PM]
starts on 12121

[8:54]
8040

[8:54]
it actually uses 2 ports

[8:54]
one is only accessible to the same computer, the other can be exposed to the world

[8:55]
continuing my example, 12121 would need to be exposed to the world, and 12122 doesn't need to be exposed.

howard [8:55 PM]
whats the localhost port?

[8:55]
is that a statically set>

[8:55]
?

zack
[8:56 PM]
it is one higher than the port you select to expose to the world

[8:56]
12122

howard [8:56 PM]
ok so in this case if not specified im opening on 8040 and localhost 8041

[8:56]
8041 is for interal api use

[8:56]
right

zack
[8:56 PM]
yes, you got it