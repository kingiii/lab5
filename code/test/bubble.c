#include <stdio.h>
#include <stdlib.h>


int initVtables () {

}

void main(int argc, char* argv[]){
    int junk_1;
    int * st;
    int i;
    int j;
    int temp;
    int * junk_0;
    initVtables ();
    junk_1 = initVtables();
    i = 0;
    j = 0;
    junk_0 = malloc(4 * 10);
    st = junk_0;
    while (i < 10){
        st[i] = 10 - i;
        i = i + 1;
    }
    i = 0;
    while (i < 10){
        j = 0;
        while (j < 10 - i - 1){
            if (st[j] > st[j + 1]){
                temp = st[j];
                st[j] = st[j + 1];
                st[j + 1] = temp;
            }
            else{
            }
            j = j + 1;
        }
        i = i + 1;
    }
    i = 0;
    while (i < 10){
        printf ("%d\n", st[i]);
        i = i + 1;
    }
}


