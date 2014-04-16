
br L0
L1:
pop -2(D1)
add #3 SP SP
push #10
pop 0(D1)
push 0(D1)
push #10
cmpeqs
push 0(D0)
push #5
cmpgts
ands
brfs L2
push #"A is >= to 10"
wrts
wrtln #""
br L3
L2:
push #"statement was false"
wrts
wrtln #""
L3:
L4:
push 0(D1)
push #5
cmpgts
brfs L5
push #"Enter value for A >> "
wrts
rd 0(D1)
push #""
wrts
wrtln #""
br L4
L5:
push #2
pop 1(D1)
push #0
pop 2(D1)
L6:
push #10
push 2(D1)
cmplts
brts L7
push 2(D1)
wrts
wrtln #""
push 1(D1)
push 2(D1)
adds
pop 1(D1)
push 2(D1)
push #1
adds
pop 2(D1)
br L6
L7:
push #"B = "
wrts
push 1(D1)
wrts
push #"\n"
wrts
wrtln #""
push #10
pop 2(D1)
L8:
push #0
push 2(D1)
cmpgts
brts L9
push 2(D1)
wrts
wrtln #""
push 1(D1)
push 2(D1)
subs
pop 1(D1)
push 2(D1)
push #1
subs
pop 2(D1)
br L8
L9:
push #"B = "
wrts
push 1(D1)
wrts
push #"\n"
wrts
wrtln #""
push 0(D0)
push 0(D1)
adds
push 1(D1)
muls
pop 0(D0)
push #"the procedure's result is: "
wrts
push 0(D0)
wrts
push #" "
wrts
push 0(D1)
wrts
push #" "
wrts
push 1(D1)
wrts
push #"\n"
wrts
mov D1 SP
pop D1
ret
L0:
add #2 SP SP
mov D0 -1(SP)
mov SP D0
add #2 SP SP
push #"Enter a number >> "
wrts
rd 0(D0)
push #"Results:"
wrts
wrtln #""
push #1
pop 1(D0)
push 0(D0)
wrts
push #" "
wrts
push 1(D0)
wrts
push #"\n"
wrts
add #2 SP SP
mov D1 -1(SP)
mov SP D1
push 0(D0)
push 1(D0)
call L1
push 0(D0)
wrts
push #" "
wrts
push 1(D0)
wrts
push #"\n"
wrts
mov D0 SP
pop D0
hlt

