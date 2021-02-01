#include <Wire.h>
#include <Firmata.h>

void setup() {

  Wire.begin(4);                // join i2c bus with address #4
  //Firmata.begin(9600);
  Firmata.begin(57600);

}

void loop() {

  delay(100);

}
