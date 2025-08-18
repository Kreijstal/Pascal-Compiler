.text
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    movl $1, %ecx  # loop counter
loop:
    pushq %rcx
    movl %ecx, %edi
    call print_integer_no_newline
    popq %rcx
    addl $1, %ecx
    cmpl $6, %ecx
    jle loop
    movl $0, %eax
    leave
    ret
