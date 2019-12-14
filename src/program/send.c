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

void server (int n)
    /*
    require TRUE/\Emp
    ensure TRUE/\Emp
    */
{
    event("Ready");
    send(n);
	server(n);
}

int main ()
/*
    require TRUE/\Emp
    ensure TRUE/\Emp
    */
{
    server (5);
}

