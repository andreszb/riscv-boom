
.text
.globl _start
_start:
# Your program here:
li t5, 0x80002000
li t6, 0x80004000
li t0, 4
sw t0, 0(t5)
li t0, 0
sw t0, 0(t6)
branch:
lw t0, 0(t5)
lw t1, 0(t6)
addi t1, t1, 1
beq t0, t1, end
sw t1, 0(t6)
j branch

end:
# Write the value 1 to tohost, telling Spike to quit with an exit code of 0.
li t0, 1
la t1, tohost
sw t0, 0(t1)

# Wait for Spike to terminate
1: j 1b

# Expose tohost and fromhost to Spike so we can communicate with it.
.data
.align 6; .global tohost;   tohost:   .dword 0
.align 6; .global fromhost; fromhost: .dword 0
