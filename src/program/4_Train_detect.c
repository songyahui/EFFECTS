//https://create.arduino.cc/projecthub/Steve_Massikker/using-ir-hall-type-sensors-for-train-detection-210f53?ref=tag&ref_id=control&offset=3
bool trigger_s1, trigger_s2, latch_s1, latch_s2; 

void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
  pinMode(9, INPUT);
  pinMode(10, INPUT); 
}

void loop() {

  if (digitalRead(10) == HIGH) {
    trigger_s1 = true;
    latch_s1 = true;
  }
  else trigger_s1 = false;
    
  if (digitalRead(9) == HIGH) {
    trigger_s2 = true;
    latch_s2 = true; 
  }
  else trigger_s2 = false;  

  if (latch_s1 && latch_s2 && !trigger_s1 && !trigger_s2) {
    latch_s1 = false;    
    latch_s2 = false;     
  }

  if (latch_s1 || latch_s2) digitalWrite(LED_BUILTIN, HIGH);
  else digitalWrite(LED_BUILTIN, LOW);
  
}