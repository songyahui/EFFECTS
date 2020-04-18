//https://create.arduino.cc/projecthub/PSoC_Rocks/arduino-deep-fridge-timer-239eb5?ref=tag&ref_id=control&offset=7
volatile int S1=0;
unsigned long time_keeper=0;
unsigned long start_time=0;
unsigned long stop_time=0;


void setup()

{
 button_init();
 led_init();
 load_init();
}



void loop()

{
  while (S1==0)
  {
  state_0();
  load_control(S1);
  }
  
  
  while(S1==1)
  {
  state_1();
  load_control(S1);
  }
  
  while (S1==2)
  {
  state_2();
  load_control(S1);
  }
    
  while (S1==3)
  {
  state_3();
  load_control(S1);
  }
    
  while (S1==4)
  {
  state_4();
  load_control(S1);
  }
  
  while (S1==5)
  {
  state_5();
  load_control(S1);
  }

}


// Button Init is the function to initalize User Interrupt Switch to Select State/Mode for Load Control

void button_init(void)
{
    pinMode(2, INPUT_PULLUP);
  attachInterrupt(0, SW1, FALLING);// Interrupt for Swithc 1

}

// SW1 if the Interrupt Function Routine that controls the S1 volatile Variable
// The entire program control revolves around the S1 variable, 
// Different Value of S1 makes Different Timing for Load 

void SW1()
{
    S1++;
    if(S1>5)
    {S1=0;}
}




// init functions initializes the Output line to drive inticator LEDs and Relay

void load_init(void)
{
 pinMode(7,1);
 digitalWrite(7,0); 
  
}

void led_init(void)

{
 pinMode(A5,1);
 pinMode(A3,1);
 pinMode(A1,1);
 pinMode(13,1);
 pinMode(11,1);
 pinMode(9,1);
 
for (int j=0;j<3;j++)
{
  led_test();
}

}

// state functions drive LEDs to inform user about Timer Setting Modes

void state_0()
{
 digitalWrite(A5,1); 
 digitalWrite(A3,0);
 digitalWrite(A1,0);
 digitalWrite(13,0); 
 digitalWrite(11,0);
 digitalWrite(9,0);  
}

void state_1()
{
 digitalWrite(A5,1); 
 digitalWrite(A3,1);
 digitalWrite(A1,0);
 digitalWrite(13,0); 
 digitalWrite(11,0);
 digitalWrite(9,0);  
}
void state_2()
{
 digitalWrite(A5,1); 
 digitalWrite(A3,1);
 digitalWrite(A1,1);
 digitalWrite(13,0); 
 digitalWrite(11,0);
 digitalWrite(9,0);  
}
void state_3()
{
 digitalWrite(A5,1); 
 digitalWrite(A3,1);
 digitalWrite(A1,1);
 digitalWrite(13,1); 
 digitalWrite(11,0);
 digitalWrite(9,0);  
}
void state_4()
{
 digitalWrite(A5,1); 
 digitalWrite(A3,1);
 digitalWrite(A1,1);
 digitalWrite(13,1); 
 digitalWrite(11,1);
 digitalWrite(9,0);  
}

void state_5()
{
 digitalWrite(A5,1); 
 digitalWrite(A3,1);
 digitalWrite(A1,1);
 digitalWrite(13,1); 
 digitalWrite(11,1);
 digitalWrite(9,1);  
}

// Load control function preiodically turns ON/OFF Relay depending on Setting selected by user
// State 0 means 060 min Load On 30 min Load Off
// State 1 means 120 min Load On 30 min Load Off
// State 2 means 180 min Load On 30 min Load Off
// State 3 means 240 min Load On 30 min Load Off
// State 4 means 300 min Load On 30 min Load Off
// State 5 means Always On 

void load_control(int i)
{
  start_time = millis();
  while( (start_time+1800000*(S1*2+2) > millis()) && (i==S1) )
  {
     digitalWrite(7,1);
  }
  if (S1<5)
  {
  stop_time = millis();  
  while( (stop_time+1800000 > millis()) && (i==S1) )
  {
     digitalWrite(7,0);
  }
  }

  
}

void led_test (void)
{
  digitalWrite(A5,1); 
 digitalWrite(A3,1);
 digitalWrite(A1,1);
 digitalWrite(9,1); 
 digitalWrite(11,1);
 digitalWrite(13,1);
 delay(250);
 digitalWrite(A5,0); 
 digitalWrite(A3,0);
 digitalWrite(A1,0);
 digitalWrite(9,0); 
 digitalWrite(11,0);
 digitalWrite(13,0);
 delay(250); 
}