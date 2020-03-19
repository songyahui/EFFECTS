#include "arduino.c"
/*
Manual Linear Actuator Control using an Arduino and two pushbuttons
This demo shows how to do basic manual control of a large linear
actuator using an Arduino and two buttons. The first button extends
the actuator and the second retracts the actuator.
 The circuit:
 * RobotGeek Pushbutton - Digital Pin 2
 * RobotGeek Pushbutton - Digital Pin 4
 * RobotGeek Relay - Digital Pin 7 
 * RobotGeek Relay - Digital Pin 8 
Products Used in this demo:
 - http://www.robotgeek.com/linear-actuators
 - http://www.robotgeek.com/robotgeek-geekduino-sensor-kit
 - http://www.robotgeek.com/robotGeek-pushbutton
 - http://www.robotgeek.com/robotgeek-relay
 */
// constants won't change. They're used here to set pin numbers:
const int button1Pin = 2;     // the number of the pushbutton1 pin
const int button2Pin = 4;     // the number of the pushbutton2 pin
const int relay1Pin =  7;      // the number of the Realy1 pin
const int relay2Pin =  8;      // the number of the Relay2 pin
// variables will change:
int button1State = 0;         // variable for reading the pushbutton status
int button2State = 0;         // variable for reading the pushbutton status
const int sensorPin = 0;    // select the input pin for the potentiometer
int sensorValue = 0;  // variable to store the value coming from the sensor
void setup() { 
  
  
  //start serial connection
  //Serial.begin(9600);  
  
  // initialize the pushbutton pin as an input:
  pinMode(button1Pin, INPUT);     
  pinMode(button2Pin, INPUT);    
  // initialize the relay pin as an output:
  pinMode(relay1Pin, OUTPUT);    
  pinMode(relay2Pin, OUTPUT);    
}

void control(){
  
  
  // read the value from the sensor:
  sensorValue = analogRead(sensorPin); 
  //print out the value of the pushbutton
  //Serial.println(sensorValue);    
  
  // read the state of the pushbutton values:
  button1State = digitalRead(button1Pin);
  button2State = digitalRead(button2Pin);
  // check if the pushbutton1 is pressed.
  // if it is, the buttonState is HIGH:
  // we also ensure tha the other button is not pushed to avoid conflict
  if (button1State == HIGH && button2State == LOW) {     
    // turn relay1 on:    
    digitalWrite(relay1Pin, HIGH);  
  } 
  // When we let go of the button, turn off the relay
  else if (digitalRead(relay1Pin) == HIGH) {
    // turn relay1 off:
    digitalWrite(relay1Pin, LOW); 
  }
  
  // repeat the same procedure for the second pushbutton
  if (button1State == LOW && button2State == HIGH) {     
    // turn relay2 on:    
    digitalWrite(relay2Pin, HIGH);  
  } 
  // When we let go of the button, turn off the relay
  else if (digitalRead(relay2Pin) == HIGH) {
    // turn relay2 off:
    digitalWrite(relay2Pin, LOW); 
  }  

  control();
}


int main (){
    control();
}