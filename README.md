# kforth
A port of Jonesforth that I wrote in x86-64 assembly, circa Sept 2018.

## Lessons Learned
 - link to glibc (there's no point in making a standalone executable if you're using linux syscalls anyways, lot of wasted effort)
 - safety checks are important in real programs
 - choose registers carefully, current register choice conflicts with passing parameters

## TODOs
 - use clone() to spawn forth interpreters in seperate threads!
    - really cheap to create and destroy!
    - allocate memory dynamically for this to work well
 - ALLOC and related calls do not allocate more memory if we run out
 - profiling does not work, as we need the stdlib exit(2) to work to generate a gmon.out
 - assembler is not working yet.
 - architecture of main.S is limiting the things I can do to make this better.
    - a complete re-write would be in order.
    - use r14 and r15 to hold state
    - use sse and cmov
