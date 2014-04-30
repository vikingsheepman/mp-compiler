
br L0
L2:
pop -2(D2)
add #4 SP SP
push #5
pop 1(D2)
push 1(D2)
push 0(D2)
cmpgts
brfs L3
push 1(D2)
pop 2(D2)
br L4
L3:
push 0(D2)
pop 2(D2)
L4:
push 2(D2)
pop -3(D2)
mov D2 SP
pop D2
ret
L1:
pop -2(D1)
add #3 SP SP
push #10
pop 0(D1)
push 0(D1)
push #10
cmpgts
push 0(D1)
push #15
cmplts
ands
brfs L5
push #"10 < A < 15"
wrts
wrtln #""
br L6
L5:
push #"A <= 10 or A >= 15"
wrts
wrtln #""
L6:
push #"Enter value for A >> "
wrts
rd 0(D1)
L7:
push 0(D1)
push #5
cmpgts
brfs L8
push #"A > 5"
wrts
push #"Enter value for A >> "
wrts
rd 0(D1)
push #""
wrts
wrtln #""
br L7
L8:
push #"A <= 5"
wrts
wrtln #""
push #2
pop 1(D1)
push #0
pop 2(D1)
L9:
push #10
push 2(D1)
cmplts
brts L10
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
br L9
L10:
push #"B = "
wrts
push 1(D1)
wrts
push #"\n"
wrts
wrtln #""
push #10
pop 2(D1)
L11:
push #0
push 2(D1)
cmpgts
brts L12
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
br L11
L12:
push #"B = "
wrts
push 1(D1)
wrts
push #"\n"
wrts
wrtln #""
push #7
pop 1(D1)
push -1(D1)
push 1(D1)
pop 0(D1)
push #"the max of "
wrts
push 1(D1)
wrts
push #" and 5 is "
wrts
push 0(D1)
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

