#pragma once

#include <sys/stat.h> // For struct stat used by fstat
#include <stddef.h>
#include <errno.h>

#ifdef SYSCALLS

// The system call numbers on x86_64 Linux:
#define SYS_read   0
#define SYS_write  1
#define SYS_open   2
#define SYS_close  3
#define SYS_fstat  5

/*
 * long sys_open(const char *pathname, int flags, int mode);
 * Performs the open(2) system call.
 */
static inline long sys_open(const char *pathname, int flags, int mode)
{
    long ret;
    __asm__ volatile (
#ifdef INTEL_SYNTAX
        "mov rdi, %1 \n\t"   // fd -> rdi
        "mov rsi, %2 \n\t"   // buf -> rsi
        "mov rdx, %3 \n\t"   // count -> rdx
        "mov rax, $%c4 \n\t" // syscall number -> rax
#else
        "movq %1, %%rdi \n\t"   // 1st arg -> rdi
        "movq %2, %%rsi \n\t"   // 2nd arg -> rsi
        "movq %3, %%rdx \n\t"   // 3rd arg -> rdx
        "movq $%c4, %%rax \n\t" // syscall number -> rax
#endif
        "syscall"
        : "=a" (ret)
        : "r" (pathname), "r" ((long)flags), "r" ((long)mode), "i" (SYS_open)
        : "rdi", "rsi", "rdx", "rcx", "r11", "memory"
    );
  if (ret < 0) {
    errno = -ret;
    return -1;
  }
  return ret;
}

/*
 * long sys_read(int fd, void *buf, unsigned long count);
 * Performs the read(2) system call.
 */
static inline long sys_read(int fd, void *buf, unsigned long count)
{
    long ret;
    __asm__ volatile (
#ifdef INTEL_SYNTAX
        "mov rdi, %1 \n\t"   // fd -> rdi
        "mov rsi, %2 \n\t"   // buf -> rsi
        "mov rdx, %3 \n\t"   // count -> rdx
        "mov rax, $%c4 \n\t" // syscall number -> rax
#else
        "movq %1, %%rdi \n\t"   // fd -> rdi
        "movq %2, %%rsi \n\t"   // buf -> rsi
        "movq %3, %%rdx \n\t"   // count -> rdx
        "movq $%c4, %%rax \n\t" // syscall number -> rax
#endif
        "syscall"
        : "=a" (ret)
        : "r" ((long)fd), "r" (buf), "r" (count), "i" (SYS_read)
        : "rdi", "rsi", "rdx", "rcx", "r11", "memory"
    );
  if (ret < 0) {
    errno = -ret;
    return -1;
  }
  return ret;
}

/*
 * long sys_write(int fd, const void *buf, unsigned long count);
 * Performs the write(2) system call.
 */
static inline long sys_write(int fd, const void *buf, unsigned long count)
{
    long ret;
    __asm__ volatile (
#ifdef INTEL_SYNTAX
        "mov rdi, %1 \n\t"   // fd -> rdi
        "mov rsi, %2 \n\t"   // buf -> rsi
        "mov rdx, %3 \n\t"   // count -> rdx
        "mov rax, $%c4 \n\t" // syscall number -> rax
#else
        "movq %1, %%rdi \n\t"   // fd -> rdi
        "movq %2, %%rsi \n\t"   // buf -> rsi
        "movq %3, %%rdx \n\t"   // count -> rdx
        "movq $%c4, %%rax \n\t" // syscall number -> rax
#endif
        "syscall"
        : "=a" (ret)
        : "r" ((long)fd), "r" (buf), "r" (count), "i" (SYS_write)
        : "rdi", "rsi", "rdx", "rcx", "r11", "memory"
    );
  if (ret < 0) {
    errno = -ret;
    return -1;
  }
  return ret;
}

/*
 * long sys_close(int fd);
 * Performs the close(2) system call.
 */
static inline long sys_close(int fd)
{
    long ret;
    __asm__ volatile (
#ifdef INTEL_SYNTAX
        "mov rdi, %1 \n\t"   // fd -> rdi
        "mov rax, $%c2 \n\t" // syscall number -> rax
#else
        "movq %1, %%rdi \n\t"   // fd -> rdi
        "movq $%c2, %%rax \n\t" // syscall number -> rax
#endif
        "syscall"
        : "=a" (ret)
        : "r" ((long)fd), "i" (SYS_close)
        : "rdi", "rcx", "r11", "memory"
    );
  if (ret < 0) {
    errno = -ret;
    return -1;
  }
  return ret;
}

/*
 * long sys_fstat(int fd, struct stat *buf);
 * Performs the fstat(2) system call.
 */
static inline long sys_fstat(int fd, struct stat *buf)
{
    long ret;
    __asm__ volatile (
#ifdef INTEL_SYNTAX
        "mov rdi, %1 \n\t"   // fd -> rdi
        "mov rsi, %2 \n\t"   // buf -> rsi
        "mov rax, $%c3 \n\t" // syscall number -> rax
#else
        "movq %1, %%rdi \n\t"   // fd -> rdi
        "movq %2, %%rsi \n\t"   // buf -> rsi
        "movq $%c3, %%rax \n\t" // syscall number -> rax
#endif
        "syscall"
        : "=a" (ret)
        : "r" ((long)fd), "r" (buf), "i" (SYS_fstat)
        : "rdi", "rsi", "rcx", "r11", "memory"
    );
  if (ret < 0) {
    errno = -ret;
    return -1;
  }
  return ret;
}

#else

#define sys_open open
#define sys_read read
#define sys_write write
#define sys_close close
#define sys_fstat fstat

#endif // SYSCALLS

