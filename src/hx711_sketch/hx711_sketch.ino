#include <Firmata.h>

class HX711 {
  const uint8_t dt_pin;
  const uint8_t sck_pin;

public:
  HX711(const uint8_t dt_pin, const uint8_t sck_pin);

  void begin();

  int32_t read();
};

HX711::HX711(uint8_t dt_pin, uint8_t sck_pin) : dt_pin(dt_pin), sck_pin(sck_pin) {
}

void HX711::begin() {
  pinMode(sck_pin, OUTPUT);
  pinMode(dt_pin, INPUT);
  digitalWrite(sck_pin, LOW);
}

int32_t HX711::read() {
  while (digitalRead(dt_pin) != LOW) yield();

  int32_t value = 0;
  value = (value << 8) + shiftIn(dt_pin, sck_pin, MSBFIRST);
  value = (value << 8) + shiftIn(dt_pin, sck_pin, MSBFIRST);
  value = (value << 8) + shiftIn(dt_pin, sck_pin, MSBFIRST);
  if(value & 0x00800000) value |= 0xFF000000;

  digitalWrite(sck_pin, HIGH);
  digitalWrite(sck_pin, LOW);

  return value;
}

HX711 hx711(2, 3);

void setup() {
  // put your setup code here, to run once:
  Firmata.begin();

  hx711.begin();
}

int32_t value = 0;
byte data[4];

void loop() {
  // put your main code here, to run repeatedly:
  value = hx711.read();
  data[0] = value & 0x7F;
  value >>= 7;
  data[1] = value & 0x7F;
  value >>= 7;
  data[2] = value & 0x7F;
  value >>= 7;
  data[3] = value & 0x7F;
  Firmata.sendSysex(STRING_DATA, 4, data);
}
