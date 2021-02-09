global _main

section .text

_main:
mov RAX, 5
mov R9, 6
  mov rax, 60       ; exit(
  mov rdi, 5        ;   EXIT_SUCCESS
  syscall
