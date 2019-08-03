; this is the run-time way of checking of 2 things are equal.

(macro = (A B) '(nop A B === tuck drop drop))


