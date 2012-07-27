#include <stdio.h>
#include <stdlib.h>

struct Vtable_A{
    int (* display)();
    int (* get)();
    int (* set)();
};

struct Vtable_B{
    int (* display)();
    int (* get)();
    int (* set)();
};

struct Data_A{
    struct Vtable_A * vptr;
    int a;
};

struct Data_B{
    struct Vtable_B * vptr;
    int a;
};

struct Vtable_A obj_Vtable_A;
struct Vtable_B obj_Vtable_B;

int A_display (struct Data_A * this) {

    printf ("%d\n", this->a);
    return (0);
}

int A_get (struct Data_A * this) {

    return (this->a);
}

int A_set (struct Data_A * this, int a) {

    this->a = a;
    return (0);
}

int initVtables () {

    obj_Vtable_A.display = &A_display;
    obj_Vtable_A.get = &A_get;
    obj_Vtable_A.set = &A_set;
    obj_Vtable_B.display = &A_display;
    obj_Vtable_B.get = &A_get;
    obj_Vtable_B.set = &A_set;
}

void main(int argc, char* argv[]){
    initVtables ();
    int k;
    struct Data_B * obj_B;
    struct Data_B *junk_0;
    struct Data_B * junk_1;
    int junk_2;
    struct Data_B * junk_3;
    int junk_4;
    junk_0 = malloc(sizeof(struct Data_B));
    junk_0->vptr = & obj_Vtable_B;
    obj_B = junk_0;
    junk_1 = obj_B;
    junk_2 = junk_1->vptr->set(junk_1, 10);
    k = junk_2;
    junk_3 = obj_B;
    junk_4 = junk_3->vptr->get(junk_3);
    printf ("%d\n", junk_4);
}


