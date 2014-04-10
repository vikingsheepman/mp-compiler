
br MAIN
L0:
add #0 SP SP
push #5
pop 0(D0)
push #10
pop 8(D0)
push #20
pop 12(D0)
push 0(D0)
wrts
push #" "
wrts
push 8(D0)
wrts
push #" "
wrts
push 12(D0)
wrts
push #"\n"
wrts
mov D1 SP
pop D1
ret
MAIN:
add #36 SP SP
push #0
pop 0(D0)
push #1
pop 4(D0)
push 0(D0)
wrts
push #" "
wrts
push 4(D0)
wrts
push #"\n"
wrts
add #8 SP SP
mov D1 -4(SP)
mov SP D1
push 0(D0)
push 4(D0)
call L0
push 0(D0)
wrts
push #" "
wrts
push 4(D0)
wrts
push #"\n"
wrts
mov D0 SP
pop D0
hlt

