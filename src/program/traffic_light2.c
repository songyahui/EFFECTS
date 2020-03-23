#include "primitives.c"
#include "traffic_primitives.c"
 
void controller()
    /*
    require TRUE /\ Ready._*
    ensure TRUE /\ (Green._._)^w  //every certain position 
    */
{
    turnGreen ();    
   
    turnYellow ();
    
    turnRed();
    
    controller();
}

int main()
{
    event ("Ready");
    controller();
}