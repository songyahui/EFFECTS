#include "primitives.c"
#include "traffic_primitives.c"


void controller(int n, int m)
    /*
    require (n>0 /\ m > 0) /\ Ready.(_^*)
    ensure (n>0 /\ m > 0)  /\ (((Red^n).(Yellow^n).(Green^n))^w) 
    */
{
    red(n);
    yellow (n);
    green (n);
    controller(n, n);
}
  
   
int main()
    /*
    require TRUE /\emp
    ensure TRUE  /\ Ready.(((Red^5).(Yellow^5).(Green^5))^w) 
    */
    
{
    event ("Ready");
    controller(5, 1);
}