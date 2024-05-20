int data[2] = {327, 218}; // An array with two integers

int main() {
    // int *ptr1 = &data[0];     // Pointer to the first element
    // int *ptr2 = &data[1];     // Pointer to the second element
    
    // // Dereferencing the pointers
    // int value1 = *ptr1;
    // int value2 = *ptr2;
    asm volatile(
        "li t2, 0\n"         // Initialize sum in t2 to 0
        "li t3, 10\n"        // Set t3 to 10, the value to add each iteration
        "li t4, 10\n"        // Set t4 to 10, the loop counter
    "1:\n"
        "add t2, t2, t3\n"   // Add t3 (10) to t2 (sum)
        "addi t4, t4, -1\n"  // Decrement t4 (loop counter)
        "bnez t4, 1b\n"      // If t4 is not zero, branch to the label 1
    : // No output operands
    : // No input operands
    : "t2", "t3", "t4"     // Clobbered registers
    );
    asm volatile("lw t0, 0(%0)" : : "r"(&data[0])); // Load the first element of the array into register t0
    asm volatile(
        "li t2, 0\n"         // Initialize sum in t2 to 0
        "li t3, 10\n"        // Set t3 to 10, the value to add each iteration
        "li t4, 10\n"        // Set t4 to 10, the loop counter
    "1:\n"
        "add t2, t2, t3\n"   // Add t3 (10) to t2 (sum)
        "addi t4, t4, -1\n"  // Decrement t4 (loop counter)
        "bnez t4, 1b\n"      // If t4 is not zero, branch to the label 1
    : // No output operands
    : // No input operands
    : "t2", "t3", "t4"     // Clobbered registers
    );
    asm volatile("lw t1, 4(%0)" : : "r"(&data[1])); // Load the second element of the array into register t1
        asm volatile(
        "li t2, 0\n"         // Initialize sum in t2 to 0
        "li t3, 10\n"        // Set t3 to 10, the value to add each iteration
        "li t4, 10\n"        // Set t4 to 10, the loop counter
    "1:\n"
        "add t2, t2, t3\n"   // Add t3 (10) to t2 (sum)
        "addi t4, t4, -1\n"  // Decrement t4 (loop counter)
        "bnez t4, 1b\n"      // If t4 is not zero, branch to the label 1
    : // No output operands
    : // No input operands
    : "t2", "t3", "t4"     // Clobbered registers
    );
    return 0;
}
