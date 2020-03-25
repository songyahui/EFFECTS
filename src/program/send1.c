#include "primitives.c"

void send (int n) 
    /*
    require TRUE/\Ready.(_^*)
    ensure (n>0\/n=0)/\(Send^n).Done\/n<0/\(Send^w)
    */
{

    if (n == 0) { 
        event ("Done");
    }
    else {
        event ("Send"); 
        send (n - 1);
    }
} 

void server (int n)
    /*
    require TRUE/\emp
    ensure (n>0\/n=0)/\((Ready.(Send^n).Done)^w)
    */
{
    event("Ready");
    send(n);
	server(n);
}

int main ()
/*
    require TRUE /\emp
    ensure TRUE/\((Ready.(Send^n).Done)^w)
    */
{
    server (5);
}