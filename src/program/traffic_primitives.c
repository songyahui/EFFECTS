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


void red (int n) 
    /*
    require (n>=0)/\emp
    ensure TRUE/\(Red^n)
    */
{
    if (n == 0) { 
        turnRed();
        return;
    }
    else {
        turnRed();
        red (n - 1);
    }
} 

void green (int n) 
    /*
    require (n>=0)/\emp
    ensure TRUE/\(Green^n)
    */
{
    if (n == 0) { 
        turnGreen ();    
        return;
    }
    else {
        turnGreen ();    
        green (n - 1);
    }
} 

void yellow (int n) 
    /*
    require (n>=0)/\emp
    ensure TRUE/\(Yellow^n)
    */
{
    if (n == 0) { 
        turnYellow ();
        return;
    }
    else {
        turnYellow ();
        yellow (n - 1);
    }
} 

