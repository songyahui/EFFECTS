//https://create.arduino.cc/projecthub/valin/match-the-light-065180?ref=tag&ref_id=control&offset=8
//int potiWert = 0; // Potentiometerwert
int potiRead = A1; // Analog In des Potentiometers
int RED[] = {2, 6, 8, 10, 13}; // Pin-Belegung der 5 roten LEDs
int RED_Nr = 0; // Ausgewählte rote LED
int RED_Nr_Old = 0; // Zwischenspeicher für ausgewählte rote LED
int GREEN_Nr = 0; // Ausgewählte grüne LED
int GREEN[] = {4, 5, 7, 9, 11, 12};// Pin Belegung der 5 grünen LEDs + 1 weiße LED
int taster = A0; // Eingangs-Pin des Tasters
int tasterReset = 0; // Taster Reset --> zur Erkennung wann der Taster losgelassen wird
int gameReset = 0; // Game Reset --> zur Freigabe des Spielneustarts bei Wahl der weißen LED
int rndCounter = 0; // Zufälliger Counter zur Steuerung des Zufallsgenerator-Effekts
int gameVictory = 0; // Erfasst ob das Spiel im Sieg-Zustand ist

void setup() {
  // put your setup code here, to run once:
  for (int i = 0; i < 5; i++) { //For Schleife zur Festlegung pinModes
    pinMode(RED[i], OUTPUT);
    pinMode(GREEN[i], OUTPUT);
  }
  pinMode(GREEN[5], OUTPUT); //Grün hat eine LED mehr
  //pinMode(taster, INPUT); // Der Taster als INPUT
  Serial.begin(9600); // Startet den Serial Monitor
}

void loop() {
  // put your main code here, to run repeatedly:

  for (int i = 0; i < 5; i++) { //For Schleife zum Ausschalten aller LEDs
     if (gameVictory == 0) {digitalWrite(RED[i], LOW);} // Rote LEDs nur ausschalten, wenn nicht gerade ein Sieg gefeiert wird.
    digitalWrite(GREEN[i], LOW);
  }
  digitalWrite(GREEN[5], LOW); //Grün hat eine LED mehr zum Ausschalten

  digitalWrite(RED[RED_Nr], HIGH); // Die gewählte rote LED wird eingeschaltet
  digitalWrite(GREEN[GREEN_Nr], HIGH); // Die gewählte grüne LED wird eingeschaltet

  if (analogRead(taster) > 500 && tasterReset == 0) { // Wenn Taster gedrückt und Tasterreset freigegeben dann...

    tasterReset = 1; // Taster sperren
    Serial.println(analogRead(potiRead)); // Debug: Potiwert auslesen
    //RED_Nr = random (5);

    if ((GREEN_Nr - 1) == RED_Nr && gameReset == 1) { // Wenn die gleiche Rote und Grüne LED gewählt ist und ein Spiel läuft dann:

        for (int i = 0; i < 5; i++) {
          digitalWrite(RED[i], HIGH); // Zur Anzeige des Sieges werden alle roten LEDs erleuchtet
        }
        gameVictory = 1; // Der Sieg-Zustand wird eingestellt
 
    }
  }

  if (analogRead(taster) < 500) { // Bei Loslassen des Tasters
    tasterReset = 0; // Taster etsperren
  }

  if (analogRead(potiRead) > 500) { // Weist je nach Poti-Wert eine Grüne LED zu.

    GREEN_Nr = 5;

  } else {
    if (analogRead(potiRead) > 400) {
      GREEN_Nr = 4;
    } else {
      if (analogRead(potiRead) > 300) {
        GREEN_Nr = 3;
      } else {
        if (analogRead(potiRead) > 200) {
          GREEN_Nr = 2;
        } else {
          if (analogRead(potiRead) > 100) {
            GREEN_Nr = 1;
          } else {
            GREEN_Nr = 0;
          }
        }

      }
    }
  }

  if (GREEN_Nr == 0 && gameReset == 0) {    // Wenn gameReset auf 0 steht und die weiße LED gewählt wird, Zufallsgenerator starten
    gameReset = 1; // gameReset sperren, bis der Spieler die Aufgabe löst

  rndCounter = random(10,30);

    for (int k = 0; k < 20; k++) { // Zufalls-Effekt --> x mal andere LED ansteuern 
      RED_Nr_Old = RED_Nr; // Alte Rote LED zwischenspeichern
      do  { // stellt sicher, dass per Zufall nicht die selbe LED wieder gewählt wird
          RED_Nr = random(5);
        } while (RED_Nr_Old == RED_Nr);
        digitalWrite (RED[RED_Nr_Old], LOW);
        digitalWrite (RED[RED_Nr], HIGH);
        delay (100);
    }

  }

  if (GREEN_Nr == 0 && gameVictory == 1) { // Wenn im Sieg-Zustand die Weiße LED gewählt wird, wird der Sieg-Zustand verlassen und das Spiel zurückgesetzt.
    gameReset = 0;
    gameVictory = 0;
  }

  

}