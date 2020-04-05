#include "arduino.c"
//Control LED bar graph function based on input from switch array
//Last modified April 2019

/*
If you haven’t read our Getting Started Guide for the Arduino Uno Rev3, 
please read that first. Otherwise, continue reading. In this tutorial, 
we’ll be maxing out the digital I/O on the Arduino Uno Rev3 (Uno). We 
interface a 10-segment LED bar graph and an 8-segment switch array. 
We’ll use 9/10 LEDs on the bar graph and 5/8 switches on the switch array 
for a total of 14 pins that we’ll need on the Uno. Each LED input will 
be an output from the Uno and each switch output will be an input to the Uno.
*/

int LED0 = 0;
int LED1 = 1;
int LED2 = 2;
int LED3 = 3;
int LED4 = 4;
int LED5 = 5;
int LED6 = 6;
int LED7 = 7;
int LED8 = 8;

int switch1 = 9;
int switch2 = 10;
int switch3 = 11;
int switch4 = 12;
int switch5 = 13;

int s1status = 0;
int s2status = 0;
int s3status = 0;
int s4status = 0;
int s5status = 0;

void setup() {
  //set pins 0-8 as output
  pinMode(LED0, OUTPUT);
  pinMode(LED1, OUTPUT);
  pinMode(LED2, OUTPUT);
  pinMode(LED3, OUTPUT);
  pinMode(LED4, OUTPUT);
  pinMode(LED5, OUTPUT);
  pinMode(LED6, OUTPUT);
  pinMode(LED7, OUTPUT);
  pinMode(LED8, OUTPUT);

  //set pins 9-14 as input
  pinMode(switch1, INPUT);
  pinMode(switch2, INPUT);
  pinMode(switch3, INPUT);
  pinMode(switch4, INPUT);
  pinMode(switch5, INPUT);

}

void loop() {
  //check inputs from switches
  s1status = digitalRead(switch1);
  s2status = digitalRead(switch2);
  s3status = digitalRead(switch3);
  s4status = digitalRead(switch4);
  s5status = digitalRead(switch5);
  
  if(s1status == 1){
    //perform LED function 1 -- forward light
    
    digitalWrite(LED0, HIGH);
    delay(200);
    digitalWrite(LED0, LOW);
    digitalWrite(LED1, HIGH);
    delay(200);
    digitalWrite(LED1, LOW);
    digitalWrite(LED2, HIGH);
    delay(200);
    digitalWrite(LED2, LOW);
    digitalWrite(LED3, HIGH);
    delay(200);
    digitalWrite(LED3, LOW);
    digitalWrite(LED4, HIGH);
    delay(200);
    digitalWrite(LED4, LOW);
    digitalWrite(LED5, HIGH);
    delay(200);
    digitalWrite(LED5, LOW);
    digitalWrite(LED6, HIGH);
    delay(200);
    digitalWrite(LED6, LOW);
    digitalWrite(LED7, HIGH);
    delay(200);
    digitalWrite(LED7, LOW);
    digitalWrite(LED8, HIGH);
    delay(200);
    digitalWrite(LED8, LOW);
  }

  else if (s2status == 1){
    //perform LED function 2 -- group toggle

    //Turn ON LEDs 0-3, turn OFF 4-7
    digitalWrite(LED4, LOW);
    digitalWrite(LED5, LOW);
    digitalWrite(LED6, LOW);
    digitalWrite(LED7, LOW);
    digitalWrite(LED0, HIGH);
    digitalWrite(LED1, HIGH);
    digitalWrite(LED2, HIGH);
    digitalWrite(LED3, HIGH);
    delay(200);

    //Turn OFF LEDs 0-3, turn ON 4-7
    digitalWrite(LED0, LOW);
    digitalWrite(LED1, LOW);
    digitalWrite(LED2, LOW);
    digitalWrite(LED3, LOW);
    digitalWrite(LED4, HIGH);
    digitalWrite(LED5, HIGH);
    digitalWrite(LED6, HIGH);
    digitalWrite(LED7, HIGH);
    delay(200);
  }

  else if (s3status == 1){
    //perform LED function 3 -- backward light

    digitalWrite(LED8, HIGH);
    delay(200);
    digitalWrite(LED8, LOW);
    digitalWrite(LED7, HIGH);
    delay(200);
    digitalWrite(LED7, LOW);
    digitalWrite(LED6, HIGH);
    delay(200);
    digitalWrite(LED6, LOW);
    digitalWrite(LED5, HIGH);
    delay(200);
    digitalWrite(LED5, LOW);
    digitalWrite(LED4, HIGH);
    delay(200);
    digitalWrite(LED4, LOW);
    digitalWrite(LED3, HIGH);
    delay(200);
    digitalWrite(LED3, LOW);
    digitalWrite(LED2, HIGH);
    delay(200);
    digitalWrite(LED2, LOW);
    digitalWrite(LED1, HIGH);
    delay(200);
    digitalWrite(LED1, LOW);
    digitalWrite(LED0, HIGH);
    delay(200);
    digitalWrite(LED0, LOW);
  }

  else if (s4status == 1){
    //perform LED function 4 - forward and backward light

    //foward
    digitalWrite(LED0, HIGH);
    delay(200);
    digitalWrite(LED0, LOW);
    digitalWrite(LED1, HIGH);
    delay(200);
    digitalWrite(LED1, LOW);
    digitalWrite(LED2, HIGH);
    delay(200);
    digitalWrite(LED2, LOW);
    digitalWrite(LED3, HIGH);
    delay(200);
    digitalWrite(LED3, LOW);
    digitalWrite(LED4, HIGH);
    delay(200);
    digitalWrite(LED4, LOW);
    digitalWrite(LED5, HIGH);
    delay(200);
    digitalWrite(LED5, LOW);
    digitalWrite(LED6, HIGH);
    delay(200);
    digitalWrite(LED6, LOW);
    digitalWrite(LED7, HIGH);
    delay(200);
    digitalWrite(LED7, LOW);
    digitalWrite(LED8, HIGH);
    delay(200);
    digitalWrite(LED8, LOW);

    //backward
    digitalWrite(LED8, HIGH);
    delay(200);
    digitalWrite(LED8, LOW);
    digitalWrite(LED7, HIGH);
    delay(200);
    digitalWrite(LED7, LOW);
    digitalWrite(LED6, HIGH);
    delay(200);
    digitalWrite(LED6, LOW);
    digitalWrite(LED5, HIGH);
    delay(200);
    digitalWrite(LED5, LOW);
    digitalWrite(LED4, HIGH);
    delay(200);
    digitalWrite(LED4, LOW);
    digitalWrite(LED3, HIGH);
    delay(200);
    digitalWrite(LED3, LOW);
    digitalWrite(LED2, HIGH);
    delay(200);
    digitalWrite(LED2, LOW);
    digitalWrite(LED1, HIGH);
    delay(200);
    digitalWrite(LED1, LOW);
    digitalWrite(LED0, HIGH);
    delay(200);
    digitalWrite(LED0, LOW);
  }

  else if (s5status == 1){
    //perform LED function 5 -- Inward

    digitalWrite(LED0, HIGH);
    digitalWrite(LED7, HIGH);
    delay(200);
    digitalWrite(LED0, LOW);
    digitalWrite(LED7, LOW);
    digitalWrite(LED1, HIGH);
    digitalWrite(LED6, HIGH);
    delay(200);
    digitalWrite(LED1, LOW);
    digitalWrite(LED6, LOW);
    digitalWrite(LED2, HIGH);
    digitalWrite(LED5, HIGH);
    delay(200);
    digitalWrite(LED2, LOW);
    digitalWrite(LED5, LOW);
    digitalWrite(LED3, HIGH);
    digitalWrite(LED4, HIGH);
    delay(200);
    digitalWrite(LED3, LOW);
    digitalWrite(LED4, LOW);
    delay(200);
    
  }

  else{
    //perform default LED function if no switch is ON
    digitalWrite(LED0, LOW);
    digitalWrite(LED1, LOW);
    digitalWrite(LED2, LOW);
    digitalWrite(LED3, LOW);
    digitalWrite(LED4, LOW);
    digitalWrite(LED5, LOW);
    digitalWrite(LED6, LOW);
    digitalWrite(LED7, LOW);
  }

}

int main (){
  while (1) loop();
}