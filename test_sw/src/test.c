int data = 327;

int main() {
    int value1, value2;
    __asm__ volatile (
        "auipc a5, 0x3\n\t"
        "addi a5, a5, 338\n\t" // 338 is the offset to <data> from the current PC
        "sd a5, -24(s0)\n\t"
        "ld a5, -24(s0)\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "nop\n\t"
        "lw %0, 0(a5)\n\t"
        "sw %0, -28(s0)\n\t"
        : "=r"(value1), "=r"(value2) // Output operands
        : // Input operands
        : "a5", "memory" // Clobbered registers
    );
}
