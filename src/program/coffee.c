#include "primitives.c"

void addSugar (int n) 
    /*
    require TRUE/\(_^*).Cup
    ensure  (t>=n)/\ (Sugar#t)
    */
{

    if (n == 0) { 
        event ("Sugar");
    }
    else {
        timeout (oneSugar() , 1);
        addSugar (n - 1);
    }
} 

void makeCoffee (int n)
    /*
    require TRUE/\emp
    ensure (t<5/\t>=n/\t1<3)/\Cup.(Sugar#t).(Coffee#t1).Done
    */
{
    event("Cup");
    deadline (addSugar(n), 5);
    deadline (event("Coffee"), 3);
    event("Done");
}


int main ()
/*
    require TRUE /\emp
    ensure (t<8)/\ (((_^*).Done)#t)
    */
{
    makeCoffee (3);
}