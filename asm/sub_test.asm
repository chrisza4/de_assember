bits 16

mov ax, 6
mov bx, 0
start:
add bx, 1
sub ax, 2
jnz start
