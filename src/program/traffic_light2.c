#include "primitives.c"
#include "traffic_primitives.c"
 
void controller()
    /*
    require TRUE/\Ready.(_^*) 
    ensure TRUE /\ ((Green._._)^w)  
    */
{
    turnGreen ();    
   
    turnYellow ();
    
    turnRed();
    
    controller();
}

int main()
    /*
    require TRUE /\emp
    ensure TRUE /\ Ready.((Green._._)^w)  
    */
{
    event ("Ready");
    controller();
}