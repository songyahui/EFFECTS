#include "arduino.c"
/* 
https://github.com/joshua-scott/arduino-washing-machine/blob/master/washingmachine.c
ARDUINO WASHING MACHINE 
TASK REQUIREMENTS:

Implement a virtual washing machine with Arduino, relay, switches, led, seven segment display and motors.
- Motor1 simulates the drum of the washing machine. It should rotate whenever the machine is washing, rinsing or centrifuging. Otherwise it should be stopped.
- Motor2 simulates the water pump of the washing machine. It should rotate whenever water is pumped out of the machine. Otherwise it should be stopped.
- Switch1 is turning the machine ON/OFF.
- Switch2 simulates the sensor which is ‘1’ whenever there is enough water in the machine.
- Switch3 simulates the sensor which is ‘1’ whenever all the water is pumped out of the machine.
- Relay is ON when the machine takes water.

The routine of the washing machine goes like this:
1. Washing machine takes water for 3s
2. Washing machine is washing for 9s (The speed of the motor should be moderate)
3. Dirty water is pumped out of the machine for 3s
4. Washing machine takes water for 3s
5. Washing machine is rinsing for 5s
6. Rinsing water is pumped out of the machine for 3s
7. Washing machine is centrifuging for 5s (The speed of the motor should be high)
8. Led is blinking when the washing program is finished.
9. Washing machine can be turned OFF

*/

/* ----- OUTPUT MACROS ----- */

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


const int all[7] = {A, B, C, D, E, F, G};

/* ----- END OUTPUT MACROS ----- */

/* ----- SETUP ----- */

void setup() 
    /*
    require TRUE/\emp
    ensure TRUE/\Ready
    */
{
  // Display outputs
  pinMode(A, OUTPUT);
  pinMode(B, OUTPUT);
  pinMode(C, OUTPUT);
  pinMode(D, OUTPUT);
  pinMode(E, OUTPUT);
  pinMode(F, OUTPUT);
  pinMode(G, OUTPUT);
  // Other outputs
  pinMode(RELAY, OUTPUT);
  pinMode(YELLOW, OUTPUT);
  pinMode(PUMP, OUTPUT);
  pinMode(DRUM, OUTPUT);
  pinMode(SWITCH2, OUTPUT);
  pinMode(SWITCH3, OUTPUT);
  event ("Reday");

}

/* ----- END SETUP ----- */

/* ----- DISPLAY ----- */


void TurnOffLED(int led)
    /*
    require (led >=0) /\  LedOn._* 
    ensure TRUE/\LedOff
    */
{
  digitalWrite(led, LOW);
  event ("LedOff");
}

void TurnOnLED(int led)
    /*
    require (led >1 \/ led <8)/\ emp   
    ensure TRUE/\LedOn
    */
{
  digitalWrite(led, HIGH);
  event ("LedOn");
}

void TurnOffAllLED(int n )
    /*
    require n >= 0 /\ emp   
    ensure TRUE/\LedOff^n
    */
{
  if (n < 0) return ;
  else digitalWrite(all[n], LOW);
       event ("LedOn");
       TurnOffAllLED(n -1);
/*
  for (int i = 0; i < sizeof(all); i++){
    digitalWrite(all[i], HIGH);
  }
*/
}

void DisplayDigit(int digit, int time, int n) 
    /*
    require (digit >=1 \/ digit <10) /\ time >=0 /\ n >= 0 /\ Ready._^*   
    ensure TRUE/\(LedOn)^*.Delay^time.LedOff^n
    */
{
  event("DisplayDigit");
  switch (digit) {
    case 0:
      TurnOnLED(A);
      TurnOnLED(B);
      TurnOnLED(C);
      TurnOnLED(D);
      TurnOnLED(E);
      TurnOnLED(F);
      break;
    case 1:
      TurnOnLED(B);
      TurnOnLED(C);
      break;
    case 2:
      TurnOnLED(A);
      TurnOnLED(B);
      TurnOnLED(G);
      TurnOnLED(E);
      TurnOnLED(D);
      break;
    case 3:
      TurnOnLED(A);
      TurnOnLED(B);
      TurnOnLED(G);
      TurnOnLED(C);
      TurnOnLED(D);
      break;
    case 4:
      TurnOnLED(F);
      TurnOnLED(G);
      TurnOnLED(B);
      TurnOnLED(C);
      break;
    case 5:
      TurnOnLED(A);
      TurnOnLED(F);
      TurnOnLED(G);
      TurnOnLED(C);
      TurnOnLED(D);
      break;
    case 6:
      TurnOnLED(A);
      TurnOnLED(C);
      TurnOnLED(D);
      TurnOnLED(E);
      TurnOnLED(F);
      TurnOnLED(G);
      break;
    case 7:
      TurnOnLED(A);
      TurnOnLED(B);
      TurnOnLED(C);
      break;
    case 8:
      TurnOnLED(A);
      TurnOnLED(B);
      TurnOnLED(C);
      TurnOnLED(D);
      TurnOnLED(E);
      TurnOnLED(F);
      TurnOnLED(G);
      break;
    case 9:
      TurnOnLED(A);
      TurnOnLED(B);
      TurnOnLED(C);
      TurnOnLED(D);
      TurnOnLED(F);
      TurnOnLED(G);
      break;
    default:
      event("DisplayDigit needs an int 0-9");
      break;
  }

  delay(time);
  TurnOffAllLED(n);    // remove digit
}

/* ----- END DISPLAY ----- */

/* ----- WASHING HELPER FUNCTIONS ----- */

void turnOnDrum() {
  event("turnOnDrum");
  analogWrite(DRUM, 128);
}

void turnOnDrumFast() {
  event("turnOnDrumFast");
  analogWrite(DRUM, 255);
}

void turnOffDrum() {
  event("turnOffDrum");
  analogWrite(DRUM, 0);
}

void turnOnPump() {
  event("turnOnPump");
  analogWrite(PUMP, 255);
}

void turnOffPump() {
  event("turnOffPump");
  analogWrite(PUMP, 0);
}

void turnOnRelay() {
  event("turnOnRelay");
  digitalWrite (RELAY, HIGH);
}

void turnOffRelay() {
  event("turnOffRelay");
  digitalWrite (RELAY, LOW);
}

void setWaterStatus(int isFull) {
  if (isFull) {
    event("setWaterStatus: full");
    analogWrite(SWITCH2, 255);
    analogWrite(SWITCH3, 0);
  } else {
    event("setWaterStatus: empty");
    analogWrite(SWITCH2, 0);
    analogWrite(SWITCH3, 255);
  }
}

void turnBothSwitchesOff() {
  analogWrite(SWITCH2, 0);  // green OFF
  analogWrite(SWITCH3, 0);   // red OFF
}

void forXSeconds(int seconds) {
  // Show display for given time
  for (int i = 0; i < seconds; i++) {
    DisplayDigit(i, 1000, sizeof(all));
  }
}

void blinkLed() 
    /*
    require TRUE/\emp
    ensure TRUE/\HIGH.Delay.LOW.Delay
    */
{
  digitalWrite(YELLOW, HIGH);
  delay(1);
  digitalWrite(YELLOW, LOW);
  delay(1);
}

/* ----- END WASHING HELPER FUNCTIONS ----- */

/* ----- WASHING CYCLE FUNCTIONS ----- */

void letWaterIn(int time) {
  event("letWaterIn");
  turnOnRelay();
  forXSeconds(time);
  turnOffRelay();
  setWaterStatus(1);
}

void wash(int time) {
  event("wash");
  turnOnDrum();
  forXSeconds(time);
  turnOffDrum();
  setWaterStatus(0);
}

void pumpWaterOut(int time) {
  event("PumpWaterOut");
  turnOnPump();
  forXSeconds(time);
  turnOffPump();
  setWaterStatus(0);
}

void rinse(int time) {
  event("Rinse");
  turnOnDrum();
  forXSeconds(time);
  turnOffDrum();
}

void centrifuge(int time) {
  event("Centrifuge");
  turnOnDrumFast();
  forXSeconds(time);
  turnOffDrum();
}

/* ----- END WASHING CYCLE FUNCTIONS ----- */

/* ----- LOOP ----- */

void loop() {
  letWaterIn(3);
  wash(9);
  pumpWaterOut(3);
  letWaterIn(3);
  rinse(5);
  pumpWaterOut(3);
  centrifuge(5);
  turnBothSwitchesOff();
  while (1) {
    event("Done");
    blinkLed();
  }
}

/* ----- END LOOP ----- */

int main (){
  while (1) loop();
}