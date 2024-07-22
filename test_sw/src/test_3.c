int data1 = 327;
int data2 = 218;
int *ptr1 = &data1;
int *ptr2 = &data2;

int main() {
    int *ptr3 = &data1;
    int reg1 = *ptr3;
    for(int i = 0; i < 10; i++) {
        ptr1 = ptr3;
    }
    ptr3 = ptr2;
}
