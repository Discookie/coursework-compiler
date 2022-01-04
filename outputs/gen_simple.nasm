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
sub rsp,0D0h

._bb0:
mov rax,2
mov [rsp+0C8h],rax
mov rax,[rsp+0E0h]
mov rcx,[rsp+0E8h]
add rax,rcx
mov [rsp+0C0h],rax
mov rax,5
mov [rsp+0B8h],rax
mov rax,4
mov [rsp+0B0h],rax
mov rax,[rsp+0B8h]
mov rcx,[rsp+0B0h]
xor rdx,rdx
imul rcx
mov [rsp+0A8h],rax
mov rax,[rsp+0C0h]
mov rcx,[rsp+0A8h]
cmp rax,rcx
setg [rsp+0A0h]
mov rax,5
mov [rsp+98h],rax
mov rax,[rsp+0D8h]
mov rcx,[rsp+98h]
cmp rax,rcx
setl [rsp+90h]
mov rax,[rsp+90h]
mov rcx,[rsp+0A0h]
and rax,rcx
mov [rsp+88h],rax
mov rax,[rsp+0D8h]
mov [rsp+18h],rax
mov rax,[rsp+0C8h]
mov [rsp+10h],rax
mov al,[rsp+88h]
test al,al
je ._bb5

._bb1:
mov rax,[rsp+0D8h]
mov [rsp+38h],rax
mov rax,[rsp+0C8h]
mov [rsp+30h],rax

._bb2:
mov rax,0
mov [rsp+28h],rax
mov rax,[rsp+38h]
mov rcx,[rsp+28h]
cmp rax,rcx
setg [rsp+20h]
mov al,[rsp+20h]
test al,al
je ._bb4

._bb3:
mov rax,1
mov [rsp+80h],rax
mov rax,[rsp+38h]
mov rcx,[rsp+80h]
sub rax,rcx
mov [rsp+78h],rax
mov rax,[rsp+78h]
mov rcx,[rsp+78h]
xor rdx,rdx
imul rcx
mov [rsp+70h],rax
mov rax,[rsp+0E8h]
mov rcx,[rsp+70h]
xor rdx,rdx
imul rcx
mov [rsp+68h],rax
mov rax,[rsp+0E0h]
mov rcx,[rsp+78h]
xor rdx,rdx
imul rcx
mov [rsp+60h],rax
mov rax,3
mov [rsp+58h],rax
mov rax,[rsp+60h]
mov rcx,[rsp+58h]
add rax,rcx
mov [rsp+50h],rax
mov rax,[rsp+68h]
mov rcx,[rsp+50h]
add rax,rcx
mov [rsp+48h],rax
mov rax,[rsp+30h]
mov rcx,[rsp+48h]
add rax,rcx
mov [rsp+40h],rax
mov rax,[rsp+78h]
mov [rsp+38h],rax
mov rax,[rsp+40h]
mov [rsp+30h],rax
jmp ._bb2

._bb4:
mov rax,[rsp+38h]
mov [rsp+18h],rax
mov rax,[rsp+30h]
mov [rsp+10h],rax

._bb5:
mov rax,[rsp+18h]
mov rcx,[rsp+10h]
add rax,rcx
mov [rsp+8],rax
mov rax,[rsp+8]
mov [rsp+0D0h],rax
add rsp,0D0h
pop rax
add rsp,18h
pop rbp
ret

align 16

