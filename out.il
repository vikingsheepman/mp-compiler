
br L0
L1:
pop -2(D1)
add #4 SP SP
push #0
pop 1(D1)
L2:
push 0(D1)
push 1(D1)
cmplts
brts L3
push 2(D1)
push 1(D1)
adds
pop 2(D1)
push 1(D1)
push #1
adds
pop 1(D1)
br L2
L3:
mov D1 SP
pop D1
ret
L4:
pop -2(D1)
add #3 SP SP
rd 1(D1)
push #""
wrts
wrtln #""
L5:
push 1(D1)
push #0
cmplts
brfs L6
push #"Number is not positive. Try again >> "
wrts
wrtln #""
rd 1(D1)
br L5
L6:
push 1(D1)
pop -3(D1)
mov D1 SP
pop D1
ret
L8:
pop -2(D2)
add #4 SP SP
push 0(D2)
push 1(D2)
cmpgts
brfs L9
push 0(D2)
pop 2(D2)
br L10
L9:
push 1(D2)
pop 2(D2)
L10:
push 2(D2)
pop -3(D2)
mov D2 SP
pop D2
ret
L7:
pop -2(D1)
add #4 SP SP
push -1(D1)
push 0(D1)
push 1(D1)
pop 2(D1)
push 2(D1)
push 0(D1)
cmpeqs
brfs L11
push 1(D1)
pop 3(D1)
br L12
L11:
push 0(D1)
pop 3(D1)
L12:
push 2(D1)
wrts
push #" is greater than "
wrts
push 3(D1)
wrts
wrtln #""
mov D1 SP
pop D1
ret
L0:
add #2 SP SP
mov D0 -1(SP)
mov SP D0
add #4 SP SP
push #"Enter a positive number for val1 >> "
wrts
wrtln #""
push -1(D0)
push #0
pop 0(D0)
push -1(D0)
push 0(D0)
pop 2(D0)
push #"Enter a positive number for val2 >> "
wrts
wrtln #""
push -1(D0)
push #0
pop 1(D0)
push -1(D0)
push 1(D0)
pop 3(D0)
push #"The sum of numbers from 0 to "
wrts
push 0(D0)
wrts
push #" is "
wrts
push 2(D0)
wrts
wrtln #""
push #"The sum of numbers from 0 to "
wrts
push 1(D0)
wrts
push #" is "
wrts
push 3(D0)
wrts
wrtln #""
add #2 SP SP
mov D1 -1(SP)
mov SP D1
push 2(D0)
push 3(D0)
call L7
push #"End of test program."
wrts
wrtln #""
mov D0 SP
pop D0
hlt

