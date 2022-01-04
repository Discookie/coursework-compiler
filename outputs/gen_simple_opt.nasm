section .text

extern _b_write
extern _i_write
extern read_bool
extern read_int

global _start
_start:
call main
mov rbx, rax
mov rax, 60 ; sysexit
xor rdi, rdi
syscall

global main
main:
push rbp
mov rbp,rsp
push 0
sub rsp,28h

._bb0:
mov rax,2
mov [rsp+20h],rax
mov rax,3
mov [rsp+18h],rax
call read_int
mov [rsp+10h],rax

._bb1:
mov rdi,[rsp+20h]
mov rsi,[rsp+18h]
mov rdx,[rsp+10h]
call _iii_some_fn
mov [rsp+8],rax
nop

._bb2:
mov rdi,[rsp+8]
call _i_write
nop

._bb3:
add rsp,28h
pop rax
add rsp,0
pop rbp
ret

align 16

global _iii_some_fn
_iii_some_fn:
push rbp
mov rbp,rsp
push rdx
push rsi
push rdi
push 0
sub rsp,90h

._bb0:
mov rax,2
mov [rsp+88h],rax
mov rax,[rsp+0A0h]
mov rcx,[rsp+0A8h]
add rax,rcx
mov [rsp+80h],rax
mov rax,[rsp+80h]
mov rcx,14h
cmp rax,rcx
setg [rsp+78h]
mov rax,[rsp+98h]
mov rcx,5
cmp rax,rcx
setl [rsp+70h]
mov rax,[rsp+70h]
mov rcx,[rsp+78h]
and rax,rcx
mov [rsp+68h],rax
mov rax,[rsp+98h]
mov [rsp+28h],rax
mov rax,[rsp+88h]
mov [rsp+20h],rax
mov rax,[rsp+98h]
mov [rsp+10h],rax
mov rax,[rsp+88h]
mov [rsp+8],rax
mov al,[rsp+68h]
test al,al
je ._bb3

._bb1:
mov rax,[rsp+28h]
mov rcx,0
cmp rax,rcx
setg [rsp+18h]
mov rax,[rsp+28h]
mov [rsp+10h],rax
mov rax,[rsp+20h]
mov [rsp+8],rax
mov al,[rsp+18h]
test al,al
je ._bb3

._bb2:
mov rax,[rsp+28h]
mov rcx,1
sub rax,rcx
mov [rsp+60h],rax
mov rax,[rsp+60h]
mov rcx,[rsp+60h]
xor rdx,rdx
imul rcx
mov [rsp+58h],rax
mov rax,[rsp+0A8h]
mov rcx,[rsp+58h]
xor rdx,rdx
imul rcx
mov [rsp+50h],rax
mov rax,[rsp+0A0h]
mov rcx,[rsp+60h]
xor rdx,rdx
imul rcx
mov [rsp+48h],rax
mov rax,[rsp+48h]
mov rcx,3
add rax,rcx
mov [rsp+40h],rax
mov rax,[rsp+50h]
mov rcx,[rsp+40h]
add rax,rcx
mov [rsp+38h],rax
mov rax,[rsp+20h]
mov rcx,[rsp+38h]
add rax,rcx
mov [rsp+30h],rax
mov rax,[rsp+60h]
mov [rsp+28h],rax
mov rax,[rsp+30h]
mov [rsp+20h],rax
jmp ._bb1

._bb3:
mov rax,[rsp+10h]
mov rcx,[rsp+8]
add rax,rcx
mov [rsp],rax
mov rax,[rsp]
mov [rsp+90h],rax
add rsp,90h
pop rax
add rsp,18h
pop rbp
ret

align 16

