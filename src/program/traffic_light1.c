#include "traffic_primitives.c"
 
void controller()
    /*
    require TRUE/\Ready.(_^*) 
    ensure TRUE /\ (((_^*).Green)^w)
    */
{
    turnGreen ();    
    delay(4);
    turnYellow ();
    delay(2);
    turnRed();
    delay(6);
    controller();
}

int main()
    /*
    require TRUE /\emp
    ensure TRUE /\ (((_^*).Green)^w)
    */
{
    event ("Ready");
    controller();
}

