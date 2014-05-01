
br L0
L0:
add #2 SP SP
mov D0 -1(SP)
mov SP D0
add #4 SP SP
push #0
pop 2(D0)
L1:
push 2(D0)
push #1
adds
pop 2(D0)
push 2(D0)
wrts
push 2(D0)
push #10
cmpgts
brfs L1
L2:
push 2(D0)
push #0
cmpgts
brfs L3
push 2(D0)
push #1
subs
pop 2(D0)
push 2(D0)
wrts
br L2
L3:
push #4
pop 2(D0)
push #1
pop 3(D0)
L4:
push 2(D0)
push 3(D0)
cmplts
brts L5
push 3(D0)
wrts
push 2(D0)
wrts
push 2(D0)
push #1
adds
pop 2(D0)
push 3(D0)
push #1
adds
pop 3(D0)
br L4
L5:
mov D0 SP
pop D0
hlt

