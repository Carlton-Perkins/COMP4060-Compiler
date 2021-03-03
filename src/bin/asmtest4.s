.globl main
end:
movq %RAX, %RDI
callq _print_int
addq $16, %RSP
popq %RBP
movq $0, %RAX
ret


body:
callq _read_int
movq %RAX, 0(%RBP)
movq 0(%RBP), %RAX
movq %RAX, -8(%RBP)
addq $-79, -8(%RBP)
movq -8(%RBP), %RAX
jmp end


main:
pushq %RBP
movq %RSP, %RBP
subq $16, %RSP
jmp body
