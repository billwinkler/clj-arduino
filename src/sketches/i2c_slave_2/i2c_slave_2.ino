// Wire Slave Receiver

#include <Wire.h>


int redPin= 7;
int greenPin = 6;
int yellowPin = 5;

void setup() {
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(yellowPin, OUTPUT);

  digitalWrite(redPin, HIGH); 
  digitalWrite(greenPin, LOW);
  digitalWrite(yellowPin, HIGH);  
  
  Wire.begin(4);                // join i2c bus with address #4
  Wire.onReceive(receiveEvent); // register event
}

void loop()
{
  delay(100);
}

void receiveEvent(int howMany)
{
  int x = Wire.read();    // receive byte as an integer

  for (int i = 0; i <= 10; i++) {
    analogWrite(redPin, HIGH);
    delay(500);
    analogWrite(redPin, LOW);
    delay(500);    
  }
  
  if (x == greenPin ) {
    digitalWrite(redPin, LOW); 
    digitalWrite(greenPin, HIGH);
    digitalWrite(yellowPin, LOW);  
  } 
  else if (x == yellowPin ) {
    digitalWrite(redPin, LOW); 
    digitalWrite(greenPin, LOW);
    digitalWrite(yellowPin, HIGH);  
  }
  else {
    digitalWrite(redPin, HIGH); 
    digitalWrite(greenPin, LOW);
    digitalWrite(yellowPin, LOW);  
  }
    for (int i = 0; i <= 10; i++) {
    digitalWrite(redPin, HIGH);
    delay(500);
    digitalWrite(redPin, LOW);
    delay(500);    
  }
}
