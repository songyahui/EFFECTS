#include "arduino.c"

// L298 
#define L298_ENA 5
#define L298_IN1 6
#define L298_IN2 7


// SCRIPTS VARIABLES
int counterScheduler =0;
unsigned long timerScheduler = 0;
unsigned long timerLocal = 0;
int speedAuto = 0;


void setup() 
    /*
    require TRUE/\emp
    ensure TRUE/\Ready
    */
{

// Initializing pins
  pinMode(L298_ENA, OUTPUT);
  pinMode(L298_IN1, OUTPUT);
  pinMode(L298_IN2, OUTPUT);

// Set default direction to FORWARD
  digitalWrite(L298_IN1, HIGH);
  digitalWrite(L298_IN2, LOW); 
  event ("Reday");

}

void control(int counterScheduler) 
    /*
    require TRUE/\Ready._*
    ensure TRUE/\
    */
{

  	// Start Scheduler
    if (millis() > (timerScheduler + 1000)) {  // Tick every 1 sec
      counterScheduler++; 
      timerScheduler = millis();
    }  
    
    // ------------- SCRIPT SWING
    int brakingDelta = 5;
    int accelerateDelta = 6;

    // 1  | 0 > Time < 5 sec
    if (counterScheduler <= 5) {  
        // Start train
        if (millis() > (timerLocal + 100)) {
          if (speedAuto < 240) speedAuto = speedAuto + accelerateDelta;
          else speedAuto = 255;
          analogWrite(L298_ENA, speedAuto); 
          timerLocal = millis();
        }   
    }       
    
    // 2  | 10 sec > Time < 15 sec
    else if ((counterScheduler >= 10) && (counterScheduler <= 15)) {  // Stop train after 10 sec
        // Stop train
        if (millis() > (timerLocal + 100)) {
          if (speedAuto > 30) speedAuto = speedAuto - brakingDelta;
          else speedAuto = 0;
          analogWrite(L298_ENA, speedAuto); 
          timerLocal = millis();
        } 
    }  
    
    // 3  | Change direction
    else if (counterScheduler == 16) {  
        digitalWrite(L298_IN1, LOW);
        digitalWrite(L298_IN2, HIGH); 
    }   
    
    // 4  | 20 sec > Time < 30 sec
    else if ((counterScheduler >= 20) && (counterScheduler <= 30)) {  
        // Start train
        if (millis() > (timerLocal + 100)) {
          if (speedAuto < 240) speedAuto = speedAuto + accelerateDelta;
          else speedAuto = 255;
          analogWrite(L298_ENA, speedAuto); 
          timerLocal = millis();
        } 
    }       
    
    // 5  | 31 sec > Time < 40 sec
    else if ((counterScheduler >= 31) && (counterScheduler <= 40)) {  // Stop train
        // Stop train
        if (millis() > (timerLocal + 100)) {
          if (speedAuto > 30) speedAuto = speedAuto - brakingDelta;
          else speedAuto = 0;
          analogWrite(L298_ENA, speedAuto); 
          timerLocal = millis();
        } 
    }    
    
    // 6  | Return to Step 1
    else if (counterScheduler > 40) {
        counterScheduler = 0;   
        digitalWrite(L298_IN1, HIGH);
        digitalWrite(L298_IN2, LOW); 
  	}
    
    control (counterScheduler);
    
}

int main (){
    control(0);
}