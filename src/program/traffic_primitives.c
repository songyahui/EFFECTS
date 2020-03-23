#include "primitives.c"

void delay (int n) 
    /*
    require (n>=0)/\emp
    ensure (n>=0)/\(Delay^n)
    */
{
    if (n == 0) { 
        return;
    }
    else {
        event ("Delay"); 
        delay (n - 1);
    }
} 

void turnRed ()
    /*
    require TRUE/\emp
    ensure TRUE /\ Red
    */
{
    event ("Red"); 
    printf("X Ave. - RED; Y Blvd. - RED\n");
}

void turnGreen ()
    /*
    require TRUE/\emp
    ensure TRUE /\ Green
    */
{
    event ("Green"); 
}


void turnYellow ()
    /*
    require TRUE/\emp
    ensure TRUE /\ Yellow
    */
{
    event ("Yellow"); 
    printf("X Ave. - RED; Y Blvd. - YELLOW\n");
}
