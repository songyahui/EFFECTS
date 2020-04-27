#include "primitives.c"
#include <time.h>

#define INPUT 0
#define OUTPUT 1
#define HIGH 1
#define LOW 1
#define A0 0

#define A 2
#define B 8
#define C 6
#define D 5
#define E 4
#define F 3
#define G 7
#define RELAY 10
#define YELLOW 12
#define PUMP 9
#define DRUM 11
#define SWITCH2 0
#define SWITCH3 1

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
    int lower = 210, upper = 220; 
  
    // Use current time as  
    // seed for random generator 
    /*srand(time(0)); 
  
    return (rand() % 
           (upper - lower + 1)) + lower;
           */
    return 0;
}

int millis() {
    return 0;
}