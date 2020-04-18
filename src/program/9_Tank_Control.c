//https://create.arduino.cc/projecthub/zezarandrade/tank-control-with-arduino-a4d47f?ref=tag&ref_id=control&offset=10
// This routine let you to CONTROL level in a tank:
// Standard protocol to select instruments:
 void SendString (byte InstrNr, int MW) { 
Serial.print ('#');
Serial.print (InstrNr);
Serial.print ('M'); 
Serial.print (MW); 
Serial.print ('<');    }
 void SendString (byte InstrNr, int f, int d, int m) { 
Serial.print ('#');
Serial.print (InstrNr);
Serial.print ('M'); 
Serial.print (f);
Serial.print (d);
Serial.print (m); 
Serial.print ('<') ;}
void setup ()
 {    // initialize serial communication at 9600 bits per second:
Serial.begin (9600);   }
void loop () {
int RL = A0;    // select the input pin for the LEVEL potentiometer
int RSP = A1;    // select the input pin for the SET POINT potentiometer
int X = 8;      // select the pin for the pump control (ON - OFF)
int LEVEL = 0.0; // variable to store the value coming from RL
int SETPOINT = 0.0; // variable to store the value coming from RSP
 // declare X as an OUTPUT:
  pinMode(X, OUTPUT); 
  // read the values from RL and RSP:
  LEVEL = analogRead (RL);
  SETPOINT = analogRead (RSP);
   // compare sensor values:
int F;
int D;
int M;
  if (LEVEL < SETPOINT - 21){digitalWrite(X, HIGH);  } 
  if (LEVEL < SETPOINT) {F = 2;    } 
 if (LEVEL < SETPOINT) {M = 0;    }   
   if (LEVEL > SETPOINT + 21) {digitalWrite(X, LOW);  } 
 if (LEVEL > SETPOINT) {F = 3 ;}   
  if (LEVEL > SETPOINT) {M = 1 ;} 
float TRUE_LEVEL = LEVEL * (100.00 / 1023.00);
float TRUE_SETPOINT = SETPOINT * (100.00 / 1023.00);
  // print out the value you read:
 SendString (2, TRUE_LEVEL); // Instrument #02 – Vert_Meter
SendString (3, TRUE_LEVEL); // Instrument #03 – Tank_Meter
SendString (4, F, D, M); // Instrument #04 - LED
SendString (5, TRUE_LEVEL); // Instrument #05 – Num_Display
SendString (6, TRUE_SETPOINT); // Instrument #06 - Num_Display
SendString (12, TRUE_LEVEL); // Instrument #12 - Trend
SendString (13, TRUE_SETPOINT); // Instrument #13 - Trend
delay (500);   }