section .text
    global _start

_start:
    ; Set up the stack frame
    push rbp            ; Save the original base pointer
    mov rbp, rsp        ; Set the new base pointer to the current stack pointer
    sub rsp, 24         ; Allocate space on the stack for 3 variables (8 bytes each for x, y, and result)

    ; Assign values to x and y using offsets from rbp
    mov qword [rbp - 8], 5     ; x = 5
    mov qword [rbp - 16], 3    ; y = 3

    ; Compute the sum
    mov rax, [rbp - 8]         ; Load x into rax
    add rax, [rbp - 16]        ; Add y to rax
    mov [rbp - 24], rax        ; Store result

    ; Load the result value into rdi for exit status
    mov rdi, [rbp - 24]

    ; Clean up the stack
    mov rsp, rbp        ; Restore the stack pointer
    pop rbp             ; Restore the base pointer

    ; Exit with the value of result
    mov rax, 60         ; syscall: exit
    syscall
