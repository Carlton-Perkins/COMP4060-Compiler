.data
hw:
	.string "hello world\n"
.text
.globl main
main:
	movl $5, %eax
	ret
