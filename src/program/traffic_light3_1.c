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


void controller(int n, int m)
    /*
    require (n>=0 /\ m > 0) /\ Ready._*
    ensure (n>=0 /\ m >0)  /\ (Red^n.Yellow^m.Green^n)^w //context sensitive grammar
    */
{
    red(n);
    yellow (m);
    green (n);
    controller(n, m);
}
     
int main()
{
    event ("Ready");
    controller(5, 1);
}