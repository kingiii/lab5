#include <stdio.h>
#include <stdlib.h>

struct vtable_A{
    int (* BinSearch)();
    int (* initData)();
    int (* insertSort)();
    int (* printArray)();
};

struct data_A{
    struct vtable_A * vptr;
    int * data;
    int n;
};

struct vtable_A *obj_vtable_A;

int A_BinSearch (struct data_A* this, int key) {
    int low;
    int high;
    int mid;

    low = 0;
    high = this->n;
    while (!((low > high))){
        mid = (low + high)/2;
        if (this->data[mid]==key){
            return ((mid + 1));
        }
        else{
        }
        if (this->data[mid] > key){
            high = mid - 1;
        }
        else{
            low = mid + 1;
        }
    }
    return (0);
}

int A_initData (struct data_A* this) {
    int i;
    int * junk_17;

    junk_17 = malloc(4 * 10);
    this->data = junk_17;
    this->n = 10;
    i = 0;
    while (i < this->n){
        this->data[i] = 10 - i;
        i = i + 1;
    }
    return (0);
}

int A_insertSort (struct data_A* this) {
    int i;
    int j;
    int temp;

    i = 1;
    while (i < this->n){
        if (this->data[i] < this->data[i - 1]){
            temp = this->data[i];
            j = i - 1;
            while (temp < this->data[j] && (!((j < 0)))){
                this->data[j + 1] = this->data[j];
                j = j - 1;
            }
            this->data[j + 1] = temp;
        }
        else{
        }
        i = i + 1;
    }
    return (0);
}

int A_printArray (struct data_A* this) {
    int i;

    i = 0;
    while (i < this->n){
        printf ("%d\n", this->data[i]);
        i = i + 1;
    }
    return (0);
}

int initVtables () {

    obj_vtable_A = malloc(sizeof(struct vtable_A));
    obj_vtable_A->BinSearch = A_BinSearch;
    obj_vtable_A->initData = A_initData;
    obj_vtable_A->insertSort = A_insertSort;
    obj_vtable_A->printArray = A_printArray;
}

void main(int argc, char* argv[]){
    int junk_16;
    struct data_A* obj_A;
    int i;
    int position;
    int key;
    struct data_A* junk_0;
    struct data_A* junk_1;
    struct vtable_A* junk_2;
    int junk_3;
    struct data_A* junk_4;
    struct vtable_A* junk_5;
    int junk_6;
    struct data_A* junk_7;
    struct vtable_A* junk_8;
    int junk_9;
    struct data_A* junk_10;
    struct vtable_A* junk_11;
    int junk_12;
    struct data_A* junk_13;
    struct vtable_A* junk_14;
    int junk_15;
    initVtables ();
    junk_16 = initVtables();
    junk_0 = malloc(sizeof(struct data_A*));
    junk_0->vptr = obj_vtable_A;
    obj_A = junk_0;
    junk_1 = obj_A;
    junk_2 = junk_1->vptr;
    junk_3 = junk_2->initData(junk_1);
    i = junk_3;
    junk_4 = obj_A;
    junk_5 = junk_4->vptr;
    junk_6 = junk_5->printArray(junk_4);
    i = junk_6;
    junk_7 = obj_A;
    junk_8 = junk_7->vptr;
    junk_9 = junk_8->insertSort(junk_7);
    i = junk_9;
    junk_10 = obj_A;
    junk_11 = junk_10->vptr;
    junk_12 = junk_11->printArray(junk_10);
    i = junk_12;
    junk_13 = obj_A;
    junk_14 = junk_13->vptr;
    junk_15 = junk_14->BinSearch(junk_13, 8);
    position = junk_15;
    printf ("%d\n", position);
}


