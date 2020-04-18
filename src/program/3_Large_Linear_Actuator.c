//https://create.arduino.cc/projecthub/robotgeek-projects-team/control-a-large-linear-actuator-with-arduino-8a3953?ref=tag&ref_id=control&offset=2
/*

Linear Actuator Control using preset position
 
This demo shows how to do basic control of a large linear
actuator using an Arduino and two buttons. Each button is hard
coded with a preset position. Pressing a button will move
the actuator to that position.
 
 The circuit:
 * RobotGeek Pushbutton - Digital Pin 1
 * RobotGeek Pushbutton - Digital Pin 2
 * RobotGeek Relay - Digital Pin 4 
 * RobotGeek Relay - Digital Pin 7 
 
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
const int sensorPin = 0;    // select the input pin for the potentiometer

// variables will change:
int button1State = 0;         // variable for reading the pushbutton status
int button2State = 0;         // variable for reading the pushbutton status
int sensorValue = 0;  // variable to store the value coming from the sensor

int goalPosition = 350; 
int CurrentPosition = 0; 
boolean Extending = false;
boolean Retracting = false;

void setup() { 


  //start serial connection
  Serial.begin(9600);

  // initialize the pushbutton pin as an input:
  pinMode(button1Pin, INPUT);     
  pinMode(button2Pin, INPUT);    
  // initialize the relay pin as an output:
  pinMode(relay1Pin, OUTPUT);    
  pinMode(relay2Pin, OUTPUT);    
  
  //preset the relays to LOW
  digitalWrite(relay1Pin, LOW); 
  digitalWrite(relay2Pin, LOW); 

  
}

void loop(){
  
  // read the value from the sensor:
  CurrentPosition = analogRead(sensorPin); 

  
  // print the results to the serial monitor:
  Serial.print("Current = " );                       
  Serial.print(CurrentPosition);      
  Serial.print("\t Goal = ");      
  Serial.println(goalPosition);  
  
  // read the state of the pushbutton values:
  button1State = digitalRead(button1Pin);
  button2State = digitalRead(button2Pin);

  if (button1State == HIGH) {     
    // set new goal position
    goalPosition = 300; 
    
    if (goalPosition > CurrentPosition) {
        Retracting = false;
        Extending = true;
        digitalWrite(relay1Pin, HIGH);  
        digitalWrite(relay2Pin, LOW);  
        Serial.println("Extending");     
    }      
    else if (goalPosition < CurrentPosition) {
        Retracting = true;
        Extending = false;
        digitalWrite(relay1Pin, LOW);  
        digitalWrite(relay2Pin, HIGH); 
        Serial.println("Retracting");         
    }  
  }

  if (button2State == HIGH) {     
    // set new goal position
    goalPosition = 500; 
    
    if (goalPosition > CurrentPosition) {
        Retracting = false;
        Extending = true;
        digitalWrite(relay1Pin, HIGH);  
        digitalWrite(relay2Pin, LOW);  
        Serial.println("Extending");   
    }        
    else if (goalPosition < CurrentPosition) {
        Retracting = true;
        Extending = false;
        digitalWrite(relay1Pin, LOW);  
        digitalWrite(relay2Pin, HIGH); 
        Serial.println("Retracting");         
    }  
  }



  if (Extending = true && CurrentPosition > goalPosition) {
    //we have reached our goal, shut the relay off
    digitalWrite(relay1Pin, LOW); 
    boolean Extending = false; 
    Serial.println("IDLE");  
  }
  
  if (Retracting = true && CurrentPosition < goalPosition){
    //we have reached our goal, shut the relay off
    digitalWrite(relay2Pin, LOW); 
    boolean Retracting = false; 
    Serial.println("IDLE");  
  }


  
  
}