section .text

global main

extern printf
extern ExitProcess@4

main:
    push ebp
    mov ebp, esp
    sub esp, 4
    push dword 10
    pop edx
    mov dword [ebp-4], edx
    push dword 0
    pop eax
    test eax, eax
    jz L0
    push dword 100
    push numfmt
    call printf
    add esp, 8
    L0:
    xor eax, eax
    add esp, 4
    pop ebp
    ret

section .data
    numfmt: db "%d", 0xa, 0x0
