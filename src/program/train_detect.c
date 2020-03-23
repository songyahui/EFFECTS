#include "arduino.c"

#define LED_BUILTIN 0

int trigger_s1, trigger_s2, latch_s1, latch_s2; 

void setup() 
    /*
    require TRUE/\emp
    ensure TRUE/\Ready
    */
{
  pinMode(LED_BUILTIN, OUTPUT);
  pinMode(9, INPUT);
  pinMode(10, INPUT); 
  event ("Reday");
}

void control() {

  if (digitalRead(10) == HIGH) {
    trigger_s1 = 1;
    latch_s1 = 1;
  }
  else trigger_s1 = 0;
    
  if (digitalRead(9) == HIGH) {
    trigger_s2 = 1;
    latch_s2 = 1; 
  }
  else trigger_s2 = 0;  

  if (latch_s1 && latch_s2 && !trigger_s1 && !trigger_s2) {
    latch_s1 = 0;    
    latch_s2 = 0;     
  }

  if (latch_s1 || latch_s2) digitalWrite(LED_BUILTIN, HIGH);
  else digitalWrite(LED_BUILTIN, LOW);
  control();
}

int main (){
    control();
}

