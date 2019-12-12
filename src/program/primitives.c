#include <stdio.h>

int eq (int a, int b) 
{
    if (a == b) return 1;
    else return 0;
}

void event (char * ev){
    printf ("%s\n",ev);
}

int minus (int a, int b) {
    return a - b ;
}

int plus (int a, int b) {
    return a + b;
}