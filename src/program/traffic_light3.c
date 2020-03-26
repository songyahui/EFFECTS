#include "primitives.c"
#include "traffic_primitives.c"


void controller(int n, int m)
    /*
    require (n>0 /\ m > 0) /\ Ready.(_^*)
    ensure (n>0 /\ m > 0)  /\ (((Red^n).(Yellow^m).(Green^n))^w) 
    */
{
    red(n);
    yellow (m);
    green (n);
    controller(n, m);
}
  
   
int main()
    /*
    require TRUE /\emp
    ensure TRUE  /\ Ready.(((Red^5).(Yellow).(Green^5))^w) 
    */
    
{
    event ("Ready");
    controller(5, 1);
}