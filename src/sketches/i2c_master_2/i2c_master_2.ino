// Wire Master Writer


#include <Wire.h>

int redPin= 7;
int greenPin = 6;
int yellowPin = 5;

void setup()
{
  Wire.begin(); // join i2c bus (address optional for master)
}

byte x = greenPin;

void loop()
{
  Wire.beginTransmission(4); // transmit to device #4
  Wire.write(x);             // sends one byte  
  Wire.endTransmission();    // stop transmitting

  if (x == greenPin ) {
    x = yellowPin;
  }
  else {
    x = greenPin;
  }
  
  delay(500);
}
