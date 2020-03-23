#include "primitives.c"

#define INPUT 0
#define OUTPUT 1
#define HIGH 1
#define LOW 1

void pinMode (int a, int b){
    return ;
}

void digitalWrite (int a, int b){
    return ;
}

int  digitalRead (int a){
    return 0;
}

void delay (int n) 
    /*
    require (n>=0)/\/\emp
    ensure (n>=0)/\(Delay^n)
    */
{
    if (n == 0) { 
        return;
    }
    else {
        event ("Delay"); 
        delay (n - 1);
    }
} 

void analogWrite(int a, int b)
    /*
    require (a>=0)/\/\emp
    ensure (b=1/\HIGH) \/ (b=0/\LOW)
    */
{
    return ;
}


int  analogRead(int a){
    return 0;
}

int millis() {
    return 0;
}