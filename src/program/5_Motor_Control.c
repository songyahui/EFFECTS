//https://create.arduino.cc/projecthub/ahmedsoliman163/control-speed-and-direction-of-motor-6f09c2?ref=tag&ref_id=control&offset=4
#include <LiquidCrystal.h>
LiquidCrystal lcd(13, 12, 11, 10, 9, 8);                                                        

//pin 1 in  LCD 1 >>>>>>>>>>>>> ground
//pin 2 in  LCD 2 >>>>>>>>>>>>> vcc
//pin 3 in  LCD 3 >>>>>>>>>>>>> ground
//pin 4 in  LCD 4 >>>>>>>>>>>>> 13
//pin 5 in  LCD 5>>>>>>>>>>>>> ground                               // LCD connection
//pin 6 in  LCD 6 >>>>>>>>>>>>> 12
//pin 7 in  LCD 7 >>>>>>>>>>>>> NO
//pin 8 in  LCD 8 >>>>>>>>>>>>> NO
//pin 9 in  LCD 9 >>>>>>>>>>>>> NO
//pin 10 in LCD 10 >>>>>>>>>>>>> NO
//pin 11 in LCD 11  >>>>>>>>>>>>> 11
//pin 12 in LCD 12 >>>>>>>>>>>>> 10
//pin 13 in LCD 13 >>>>>>>>>>>>> 9
//pin 14 in LCD 14 >>>>>>>>>>>>> 8
//pin 15 in LCD 15 >>>>>>>>>>>> vcc
//pin 16 in LCD 16 >>>>>>>>>>>  ground
//******************************************************************************************************
const int In1 = A1;     //pin  5 in lm298 >>>>>>>> A1 in arduino
const int In2 = 7;      //pin  7 in lm298 >>>>>>>> 7 in arduino           // L298 motor driver connection
const int En = 6;       //pin  6 in lm298 >>>>>>>> 6(PWM) in arduino
                        // pin 2 in l298  >>>>>>>> Motor pin
                        // pin 3 in l298  >>>>>>>> Motor pin 
                        // pin 9  in l298  >>>>>>>>> 5 volt
                        // pin 4  in l298  >>>>>>>>> Motor voltage
                        //pin  8  in l298  >>>>>>>>> ground
//**********************************************************************************************************
const int Pot = A0;        //pin at middle in variable resistance >>>>>>>> A0 in arduino          // variable resistance  connection
                           // others pin to VCC & Ground  
//**************************************************************************************************************
const int SW = 5;          //vcc (5 Volt )  switch for direction with (resistance 10 k to ground )  >>>>>>>>>>>> 5 in arduino     // switch connection

//**********************************************************************************************************************
const int red_led = 2;      // led with resistance 330 ohm to >>>>>>>>>> 2 in arduino                 
const int yellow_led = 3;   // led with resistance 330 ohm to >>>>>>>>>> 3 in arduino                 //LED connection 
const int green_led = 4;   // led with resistance 330 ohm to >>>>>>>>>> 4 in arduino                  
//***********************************************************************************************************************
volatile float pot_read = 0.0;
volatile int i = 0;
int flag = 1;
//**********************************************************************************************
void red() {
  digitalWrite(red_led, HIGH);                         // open RED LED 
  digitalWrite(yellow_led, LOW);
  digitalWrite(green_led, LOW);
}
//*********************************************
void yellow() {
  digitalWrite(red_led, LOW);                           // open yellow LED 
  digitalWrite(yellow_led, HIGH);
  digitalWrite(green_led, LOW);
}
//**********************************************************************************
void green() {                                             
  digitalWrite(red_led, LOW);                                 // open Green  LED 
  digitalWrite(yellow_led, LOW);
  digitalWrite(green_led, HIGH);
}
//*****************************************************************************
void CW() {
  digitalWrite(In1, HIGH);
  digitalWrite(In2, LOW);                                   // Make motor run in Clock wise direction 
  Serial.println(" Clock Wise ");
}
//*********************************************************************************
void CCW() {
  digitalWrite(In2, HIGH);
  digitalWrite(In1, LOW);                                           // Make motor run in counter  Clock wise direction 
  Serial.println("counter Clock Wise ");
}
//****************************************************************************
void STP() {
  digitalWrite(In1, LOW);
  digitalWrite(In2, LOW);                                         // Make motor STOP 
  Serial.println(" STOP ");
}
//************************************************************************************
//Make input and output pins 
void setup() {
  // put your setup code here, to run once:
  lcd.begin(16, 2);
   lcd.print("Speed Control");
  delay(1000);
  pinMode(In1, OUTPUT);
  pinMode(In2, OUTPUT);
  pinMode(En, OUTPUT);
  pinMode(red_led, OUTPUT);
  pinMode(yellow_led, OUTPUT);
  pinMode(green_led , OUTPUT);
  pinMode(SW, INPUT);
  pinMode(Pot, INPUT);
  Serial.begin(9600);
}
//*******************************************************************************
void loop() {
  pot_read = analogRead(Pot);                            // read variable resistance
  pot_read = pot_read / 4.0;
  pot_read = pot_read / 254.0;
  pot_read = pot_read * 100.0;

  delay(20);

  analogWrite(En, pot_read);
  lcd.setCursor(5, 1);

  lcd.print("PWM =");
  lcd.print(pot_read);
  lcd.print("%");


  Serial.print("PWM =");
  Serial.print(pot_read);


  Serial.println("%");

  //  Serial.print(" pot_read= ");
  //  Serial.println( pot_read );

  if (digitalRead(SW) == 1)
  {
    i++;
    switch (i)
    {
      case 1:
        CW();
        lcd.setCursor(0, 1);

        lcd.print(" CW");

        green();
        Serial.println(" Clock Wise ");
        break;
      case 2:
        CCW();
        lcd.setCursor(0, 1);

        lcd.print("CCW");
        yellow();
        Serial.println("counter Clock Wise ");
        break;
      case 3:
        STP();
        red();
        lcd.setCursor(0, 1);

        lcd.print("STP");
        Serial.println(" STOP ");
        i = 0;
        break;
    }
    while (digitalRead(SW) == 1);
  }

}