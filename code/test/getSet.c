#include <stdio.h>
#include <stdlib.h>

struct vtable_A{
    int (* display)();
    int (* get)();
    int (* set)();
};

struct data_A{
    struct vtable_A * vptr;
    int a;
};

struct vtable_A *obj_vtable_A;

int A_display (struct data_A* this) {

    printf ("%d\n", this->a);
    return (0);
}

int A_get (struct data_A* this) {

    return (this->a);
}

int A_set (struct data_A* this, int a) {

    this->a = a;
    return (0);
}

int initVtables () {

    obj_vtable_A = malloc(sizeof(struct vtable_A));
    obj_vtable_A->display = A_display;
    obj_vtable_A->get = A_get;
    obj_vtable_A->set = A_set;
}

void main(int argc, char* argv[]){
    int junk_7;
    int k;
    struct data_A* obj_A;
    struct data_A* junk_0;
    struct data_A* junk_1;
    struct vtable_A* junk_2;
    int junk_3;
    struct data_A* junk_4;
    struct vtable_A* junk_5;
    int junk_6;
    initVtables ();
    junk_7 = initVtables();
    junk_0 = malloc(sizeof(struct data_A*));
    junk_0->vptr = obj_vtable_A;
    obj_A = junk_0;
    junk_1 = obj_A;
    junk_2 = junk_1->vptr;
    junk_3 = junk_2->set(junk_1, 10);
    k = junk_3;
    junk_4 = obj_A;
    junk_5 = junk_4->vptr;
    junk_6 = junk_5->get(junk_4);
    printf ("%d\n", junk_6);
}


