#include "primitives.c"
#include "traffic_primitives.c"
 
void controller()
    /*
    require TRUE /\ Ready._*
    ensure TRUE /\ (_*.Green)^w 
    */
{
    turnGreen ();    
    delay(5);
    turnYellow ();
    delay(2);
    turnRed();
    delay(2);
    turnRed();
    delay(2);
    turnRed();
    delay(2);
    turnRed();
    delay(2);
    controller();
}

int main()
{
    event ("Ready");
    controller();
}