//https://create.arduino.cc/projecthub/Steve_Massikker/arduino-train-demo-2-67d8a0?ref=tag&ref_id=control&offset=5
// L298
#define IN1_PIN 4
#define IN2_PIN 5
#define ENA_PIN 9

// VARIABLES //
bool flag_LED = false;
bool stringComplete = false;
String inputString = ""; 

void setup() {

// Initializing Serial
  Serial.begin(9600);
  inputString.reserve(4); 

// Initializing Motor-Driver
  pinMode(ENA_PIN, OUTPUT); 
  pinMode(IN1_PIN, OUTPUT); 
  pinMode(IN2_PIN, OUTPUT);
  pinMode(LED_BUILTIN, OUTPUT);
  
// Set PWM frequency for D9 & D10
// Timer 1 divisor to 256 for PWM frequency of 122.55 Hz
  TCCR1B = TCCR1B & B11111000 | B00000100;    

}

void loop() {

  if (stringComplete) {

    if (inputString.charAt(0) =='a') {

      //THROTTLE 
      // Speed 
      if (inputString.charAt(1) =='0') {
        if (inputString.charAt(2) =='0') analogWrite(ENA_PIN, 0);
        if (inputString.charAt(2) =='2') analogWrite(ENA_PIN, 60);
        if (inputString.charAt(2) =='4') analogWrite(ENA_PIN, 80);
        if (inputString.charAt(2) =='6') analogWrite(ENA_PIN, 100);
        if (inputString.charAt(2) =='8') analogWrite(ENA_PIN, 120);
      } 
      if (inputString.charAt(1) =='1') {
        if (inputString.charAt(2) =='0') analogWrite(ENA_PIN, 140);
        if (inputString.charAt(2) =='2') analogWrite(ENA_PIN, 170);
        if (inputString.charAt(2) =='4') analogWrite(ENA_PIN, 200);
        if (inputString.charAt(2) =='6') analogWrite(ENA_PIN, 230);
        if (inputString.charAt(2) =='8') analogWrite(ENA_PIN, 255);
      }      

      // DIRECTION
      if (inputString.charAt(1) =='d') {
        if (inputString.charAt(2) =='f') { // (f) Forward
          digitalWrite(IN1_PIN, HIGH);
          digitalWrite(IN2_PIN, LOW);
        }
        if (inputString.charAt(2) =='b') { // (b) Backward
          digitalWrite(IN1_PIN, LOW);
          digitalWrite(IN2_PIN, HIGH);
        }
        if (inputString.charAt(2) =='s') { // (s) Stop button
          digitalWrite(IN1_PIN, LOW);
          digitalWrite(IN2_PIN, LOW);
          analogWrite(ENA_PIN, 0);
          flag_LED = !flag_LED;
        }
      }
    }

    inputString = "";
    stringComplete = false;
  }

  if (flag_LED) digitalWrite(LED_BUILTIN, HIGH);
  else digitalWrite(LED_BUILTIN, LOW);
}

void serialEvent() {
  while (Serial.available()) {
    char inChar = (char)Serial.read();
    inputString += inChar;
    if (inChar == 'z') {
      stringComplete = true;
    }
  }
}