# kforth
A port of Jonesforth that I wrote in x86-64 assembly, circa Sept 2018.

use clone() to spawn forth interpreters in seperate threads!
( really cheap to create and destroy! )

we should allocate memory dynamically for this to work well

ALLOC and related calls do not allocate more memory if we run out...

profiling does not work for this, as we need the stdlib exit(2) to work to generate a gmon.out

assembler is not working yet.

architecture of main.S is limiting the things I can do to make this better.
a complete re-write would be in order.
	use r14 and r15 to hold state
	use sse and cmov

Things to do differently the second time:
	link to glibc (no point in making a standalone executable if you're using linux syscalls anyways, lot of wasted effort)
		probably smaller (ended up being larger, ~19K vs 24K without -nostdlib)
	use autotools to check platform compliance, better than hacked together shell script
	make interpreter better (more safety checks)
	better choice of registers, register choice conflicts with parameter passing
