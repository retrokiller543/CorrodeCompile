section .text
	global _start

_start:
push rbp
mov rbp, rsp
sub rsp, 40

; user code
push 5
pop rax
mov [rsp-8], rax
push 3
pop rax
mov [rsp-16], rax
push 2
pop rax
mov [rsp-24], rax
mov rax, [rsp-24]
push rax
mov rax, [rsp-8]
push rax
mov rax, [rsp-16]
push rax
pop rdi
pop rax
add rax, rdi
push rax
pop rdi
pop rax
imul rax, rdi
push rax
push 2
pop rdi
pop rax
imul rax, rdi
push rax
pop rax
mov [rsp-32], rax
mov rax, [rsp-32]
push rax
mov rax, [rsp-16]
push rax
pop rdi
pop rax
sub rax, rdi
push rax
pop rax
mov [rsp-40], rax

mov rsp, rbp
pop rbp
; exit program
mov rdi, rax
mov rax, 60
syscall
