#include "primitives.c"
#include "traffic_primitives.c"

void red (int n) 
    /*
    require (n>=0)/\/\Ready.(_^*)
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
    require (n>=0)/\/\Ready.(_^*)
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
    require (n>=0)/\/\Ready.(_^*)
    ensure TRUE/\(Green^n)
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


void controller(int n)
    /*
    require (n>=0 ) /\ Ready._*
    ensure (n>=0 )  /\ (Red^n.Yellow^n.Green^n)^w
    */
{
    red(n);
    yellow (n);
    green (n);
    controller(n);
}
     
int main()
{
    event ("Ready");
    controller(5);
}