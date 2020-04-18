//This code is to use with dual axis fpv camera cradle that uses two servos, adding a joystick module
//We control the position of the cradle by moving the analog stick, the cradle move to the direction where the stick is pointing
//until the limits and stay there
//Refer to surtrtech.com for more information
//https://create.arduino.cc/projecthub/SurtrTech/control-dual-axis-fpv-camera-cradle-with-joystick-module-6ee514?ref=tag&ref_id=control&offset=1

#include <Servo.h> //Servos library and declaration

Servo myservo1;
Servo myservo2;

int a,b,X,Y; //Variables needed later
int YAxis = 1; //Declaring where the X axis and Y axis of the joystick pins are wired
int XAxis = 0; //Of course analog inputs

void setup() {
 Serial.begin(9600); //Setting the Serial monitor baude rate and launching
 pinMode(XAxis, INPUT); //Declaring the pin modes and servo pins
 myservo1.attach(8);
 pinMode(YAxis, INPUT); 
 myservo2.attach(9);
}

void loop() {
 
 a=myservo1.read(); //Reading the previous servos positions is an important step so we can know where they should position next
 b=myservo2.read();
 X=analogRead(XAxis);//Reading the joystick values
 Y=analogRead(YAxis);
 
 if(X>550){ //Here we didn't do any calibration so the joystick has three positions (Left|Resting|Right)
 a=a-1; //it depends on the value we read we can know in which direction the stick is pointing and I left the resting position big actually it's just 1 value
 myservo1.write(a); //we inject the new value
 delay(50); //You can make the delay big or short or act on a=a-x to make big steps or short steps
 }
 if(X<450){
 a=a+1; //Here we did the opposit operation to move to the opposit direction
 myservo1.write(a);
 delay(50);
 }
 if(Y>600){ //Here we didn't do any calibration so the joystick has three positions (Up|Resting|Down) ditto
 b=b+1;
 myservo2.write(b);
 delay(50);
 }
 if(Y<450){
 b=b-1;
 myservo2.write(b);
 delay(50);
 }