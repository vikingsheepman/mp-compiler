br L1

L2:
pop 12(D0)
add #4 SP SP
mov D1 -16(SP)
sub SP #16 D1

mov #5 12(D1)
mov #10 4(D1)
mov #20 8(D1)

wrt 12(D1)
wrt #" "
wrt 4(D1)
wrt #" "
wrt 8(D1)
wrt #" "

add #4 D1 D1
mov D1 SP
pop D1

ret


L1:
add #4 SP SP
add #8 SP SP
mov D0 -12(SP)
sub SP #12 D0

mov #0 4(D0)
mov #1 8(D0)

wrt 4(D0)
wrt #" "
wrt 8(D0)
wrt #" "

add #8 SP SP
push 4(D0)
push 8(D0)
call L2

wrt 4(D0)
wrt #" "
wrt 8(D0)
wrt #" "

wrtln #""

add #4 D0 D0
mov D0 SP
pop D0

hlt