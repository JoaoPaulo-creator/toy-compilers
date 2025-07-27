section .text
global main
extern printf

main:
  push rbp
  mov rbp, rsp
  sub rsp, 1024
  lea rax, [arr1_data]
  mov [rbp - 8], rax
  mov rax, [rbp - 8]
  mov rax, [rax]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  mov rax, [rbp - 8]
  mov rbx, rax
  mov rax, 2
  mov rcx, rax
  mov rax, [rbx + rcx*8 + 8]
  mov [rbp - 16], rax
  mov rax, [rbp - 16]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  mov rax, [rbp - 16]
  push rax
  mov rax, [rbp - 8]
  mov rbx, rax
  mov rax, 4
  mov rcx, rax
  mov rax, [rbx + rcx*8 + 8]
  mov rbx, rax
  pop rax
  add rax, rbx
  mov [rbp - 24], rax
  mov rax, [rbp - 24]
  push rax
  mov rax, 7
  mov rbx, rax
  pop rax
  cmp rax, rbx
  setg al
  movzx rax, al
  mov [rbp - 32], rax
  mov rax, [rbp - 32]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  mov rax, [rbp - 32]
  cmp rax, 0
  sete al
  movzx rax, al
  mov [rbp - 40], rax
  mov rax, [rbp - 40]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  mov rax, [rbp - 24]
  push rax
  mov rax, 8
  mov rbx, rax
  pop rax
  cmp rax, rbx
  sete al
  movzx rax, al
  cmp rax, 0
  je Lelse2
  mov rax, [rbp - 24]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  jmp Lend3
Lelse2:
  mov rax, 0
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
Lend3:
  mov rax, 0
  mov [rbp - 48], rax
Lwhile4:
  mov rax, [rbp - 48]
  push rax
  mov rax, [rbp - 8]
  mov rax, [rax]
  mov rbx, rax
  pop rax
  cmp rax, rbx
  setl al
  movzx rax, al
  cmp rax, 0
  je Lwend5
  mov rax, [rbp - 8]
  mov rbx, rax
  mov rax, [rbp - 48]
  mov rcx, rax
  mov rax, [rbx + rcx*8 + 8]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  mov rax, [rbp - 48]
  push rax
  mov rax, 1
  mov rbx, rax
  pop rax
  add rax, rbx
  mov [rbp - 48], rax
  jmp Lwhile4
Lwend5:
  mov rax, 0
  mov [rbp - 56], rax
Lfor6:
  mov rax, [rbp - 56]
  push rax
  mov rax, 3
  mov rbx, rax
  pop rax
  cmp rax, rbx
  setl al
  movzx rax, al
  cmp rax, 0
  je Lfend7
  mov rax, [rbp - 56]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  mov rax, [rbp - 56]
  push rax
  mov rax, 1
  mov rbx, rax
  pop rax
  add rax, rbx
  mov [rbp - 56], rax
  jmp Lfor6
Lfend7:
  mov rax, [rbp - 8]
  mov rax, [rax]
  mov rdi, fmt_int
  mov rsi, rax
  xor rax, rax
  call printf
  mov rsp, rbp
  pop rbp
  mov rax, 0
  ret

section .data
fmt_int: db "%d", 10, 0
arr1_data: dq 5, 1, 2, 3, 4, 5
