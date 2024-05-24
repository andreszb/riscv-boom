int data1 = 327;
int data2 = 218;
int *ptr1 = &data1;
int *ptr2 = &data2;

int main() {
    // int value1, value2;
    // __asm__ volatile (
    //     "auipc a5, 0x3\n\t"      // add upper immediate to pc
    //     "addi a5, a5, 338\n\t"
    //     "sd a5, -24(s0)\n\t"
    //     "ld a5, -24(s0)\n\t"
    //     "lw %0, 0(a5)\n\t"
    //     "sw %0, -28(s0)\n\t"
    //     // "la a5, data\n\t"        // load address of data
    //     "li a4, 218\n\t"         // load immediate value 218 into a4
    //     "sw a4, 0(a5)\n\t"       // store the value 218 into the address pointed by a5 (data)
    //     : "=r"(value1)           // output registers
    //     :                        // no input registers
    //     : "a4", "a5", "memory"   // clobbered registers
    // );
    int *ptr3 = &data1;     // Pointer to the first element
    int value1 = *ptr3;
    ptr3 = ptr2;
    
}
