#include "primitives.c"
#include "traffic_primitives.c"

void controller()
    /*
    require TRUE /\ Ready._*
    ensure TRUE /\ (_._.Green)^w
    */
{
    turnYellow ();
    delay (2);
    turnRed();
    delay (2);
    turnGreen (); 
    delay (2);   
    controller();
}

int main()
{
    event ("Ready");
    controller();
}

