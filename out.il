
br L0
L1:
pop -2(D1)
add #2 SP SP
push 0(D1)
push #1
adds
pop 0(D1)
push 0(D1)
pop -3(D1)
mov D1 SP
pop D1
ret
L2:
pop -2(D1)
add #2 SP SP
push 0(D1)
push #1
adds
pop 0(D1)
push 0(D1)
pop -3(D1)
mov D1 SP
pop D1
ret
L0:
add #2 SP SP
mov D0 -1(SP)
mov SP D0
add #2 SP SP
push #10
pop 0(D0)
push #20
pop 1(D0)
add #3 SP SP
mov D1 -1(SP)
mov SP D1
push 0(D0)
call L2
pop 1(D0)
add #3 SP SP
mov D1 -1(SP)
mov SP D1
push 1(D0)
call L1
pop 1(D0)
push 1(D0)
push #1
adds
wrts
wrtln #""
push 0(D0)
wrts
wrtln #""
mov D0 SP
pop D0
hlt

