
.text
.globl _start
_start:
# Your program here:
li t3, 0
li t4, 10
li t0, 1
li t1, 2
add t2, t1, t0
branch:
add t0, t0, 1
add t1, t1, 2
add t2, t1, t0
addi t3, t3, 1
bne t3, t4, branch


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
