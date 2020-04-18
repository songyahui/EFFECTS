  
  //https://create.arduino.cc/projecthub/basem/modified-aquariumatic-management-system-ams-iot-app-02ddfe?ref=tag&ref_id=control&offset=15
  /*************
  
    The Islamic University of Gaza Faculty of Engineering
    Electrical Engineering Department 
    Aquariumatic management system (AMS - IoT App)

    A graduation project is submitted to the Electrical Engineering Department in partial fulfillment of the requirements for the degree of B.Sc. in Electrical Engineering!
    By : Basem M. Khalaf     basem.khalaf.1994@ieee.org
         Hazem E. Abu Taha
    Supervisor : Dr. Jawdat Abu Taha

    Gaza, Palestine 
    15 May 2017
    You can find more about this project , just click on the Link : 
    or you can contact with Basem by this email : basem.khalaf.1994@ieee.org
    
    Note :
    Typical Ethernet Module wiring would be:
 *  VCC -- 5V
 *  GND -- GND
 *  CS  -- D10
 *  SI  -- D11
 *  SCK -- D13
 *  SO  -- D12
 *  INT -- D2
 
 **************/
 
#define BLYNK_PRINT Serial           // Comment this out to disable prints and save space
#include <SPI.h>
#include <UIPEthernet.h>
#include <BlynkSimpleUIPEthernet.h>
#include <SimpleTimer.h>

                                     // You should get Auth Token in the Blynk App.
                                     // Go to the Project Settings (nut icon).
char auth[] = "ef83d5daf43e406987fca44a0a90015d";
#define echoPin 8                    // Echo Pin
#define trigPin 9                    // Trigger Pin
long duration, distance;             // Duration used to calculate distance
float temp;
int tempPin = 1;                     //analog pin 1
SimpleTimer  timer;                  // http://playground.arduino.cc/Code/SimpleTimer , simply how to use timer in your code to send data over time intervals !

    
void setup()
{
  Serial.begin(9600);
  Blynk.begin(auth);
  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
  timer.setInterval(1500, sendUptime);                    //i removed L from 1000L! more time to send less flood on to Blynk server !
  timer.setInterval(1500, sendUpdestance);                // USonic sensor data sending!
  
                                                          //You can also specify server.
                                                          //For more options, see Boards_Ethernet/Arduino_Ethernet_Manual example
                                                          //Blynk.begin(auth, "blynk-cloud.com", 8442);
                                                          //Blynk.begin(auth, IPAddress(192,168,1,100), 8888);
}
 void sendUpdestance()               // shows the value temp on virtual pin 10
 {
  Blynk.virtualWrite(8, distance);                  
 }
                                     // that you define how often to send data to Blynk App.
 void sendUptime()
    {
                                                           // shows the value temp on virtual pin 10
      Blynk.virtualWrite(10, temp); 
    }

void loop()
{
  
  /* The following trigPin/echoPin cycle is used to determine the
 distance of the nearest object by bouncing soundwaves off of it. */ 
 digitalWrite(trigPin, LOW); 
 delayMicroseconds(2); 

 digitalWrite(trigPin, HIGH);
 delayMicroseconds(10); 
 
 digitalWrite(trigPin, LOW);
 duration = pulseIn(echoPin, HIGH);
 
 //Calculate the distance (in cm) based on the speed of sound.
 distance = duration/58.2;
 Serial.println(distance);
 delay(500);
 Blynk.run();                                              //Initiates Blynk server 
  timer.run();                                              // Initiates SimpleTimer
  temp = analogRead(tempPin);
  temp = (temp * 0.48828125)/100;
}