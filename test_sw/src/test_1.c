int data[2] = {100, 200}; // An array with two integers

int main() {
    int *ptr1 = &data[0];     // Pointer to the first element
    int *ptr2 = &data[1];     // Pointer to the second element
    
    // Dereferencing the pointers
    int value1 = *ptr1;
    int value2 = *ptr2;
    return 0;
}
