#include "primitives.c"

void send (int n) 
    /*
    require TRUE/\Emp
    ensure (n>0\/n=0)/\(Send^n).Done\/n<0/\Send^w
    */
{

    if (n == 0) { 
        event ("Done");
    }
    else {
        event ("Send"); 
        send (n- 1);
    }
} 

int main ()
    /*
    require TRUE/\Emp
    ensure TRUE/\Emp
    */
{
    send(5);
}

