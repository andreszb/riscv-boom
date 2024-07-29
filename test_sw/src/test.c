int data[5] = {100, 200, 300, 400, 500};

int main() {

    int *ptr[5];
    for (int i = 0; i < 5; i++) {
        ptr[i] = &data[i];
    }

    // Pointers to pointers
    int **pptr[5];
    for (int i = 0; i < 5; i++) {
        pptr[i] = &ptr[i];
    }

    // Loop and dereference pointers to different addresses
    for (int j = 0; j < 10; j++) {
        // Dereference pointers
        int value1 = *ptr[j % 5];
        int value2 = *ptr[(j + 1) % 5];
        int value3 = *ptr[(j + 2) % 5];
        int value4 = *ptr[(j + 3) % 5];
        int value5 = *ptr[(j + 4) % 5];

        // Deref pptr
        int value6 = **pptr[j % 5];
        int value7 = **pptr[(j + 1) % 5];
        int value8 = **pptr[(j + 2) % 5];
        int value9 = **pptr[(j + 3) % 5];
        int value10 = **pptr[(j + 4) % 5];

        // Update ptr values
        *ptr[j % 5] = 789 + j;
        *ptr[(j + 1) % 5] = 456 + j;
        *ptr[(j + 2) % 5] = 123 + j;
        *ptr[(j + 3) % 5] = 678 + j;
        *ptr[(j + 4) % 5] = 234 + j;

        // Update pptr values
        **pptr[j % 5] = 987 + j;
        **pptr[(j + 1) % 5] = 654 + j;
        **pptr[(j + 2) % 5] = 321 + j;
        **pptr[(j + 3) % 5] = 876 + j;
        **pptr[(j + 4) % 5] = 543 + j;
    }
}
