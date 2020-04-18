//https://create.arduino.cc/projecthub/dokaniaharsh/t-rex-run-chrome-game-controller-042483?ref=tag&ref_id=control&offset=0
int val=0;
int laststate=0;
void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
pinMode(A0,INPUT);
}

void loop() {
  val=analogRead(A0);
  Serial.println(val);
  laststate=0;
  while(val>270 && val<400)
  {
   val=analogRead(A0);
   laststate=1;
  }
  if(laststate==1)
  {  Serial.println(1);
     delay(10); 
  }
}