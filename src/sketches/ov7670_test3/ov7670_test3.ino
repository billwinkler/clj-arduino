
uint16_t vcnt = 0;
uint16_t pcnt = 0;
uint16_t bcnt = 0;
volatile byte sync = 0;
volatile byte flag = 0;
volatile byte data = 0;
volatile byte valid = 0;
uint8_t buf1[128];
uint8_t buf2[128];
uint8_t*b=buf1;
uint8_t*b2=buf1;

uint8_t*b7;       // a pointer to an unsigned byte type
uint8_t*last_b7;  // buffer read/write indicator

void pclk() {
  // reset buffer pointer while vsync is high
  if (PIND&8) {
    ++vcnt;
    bcnt = 0;
    if (last_b7 == &buf1[0]) {
      b7 = &buf2[0];
    } else {
      b7 = &buf1[0];
    }
    last_b7 = b7;   
  }
  // write the data to the buffer
  *b7 =(PINC&15)|(PIND&240);

  if ((b7 - last_b7) < sizeof(buf1)) {
    ++b7; 
  } else {
    // increment buffer counter
    ++bcnt;    
    if (last_b7 == &buf1[0]) {
      b7 = &buf2[0];
    } else {
      b7 = &buf1[0];
    }
    last_b7 = b7; 
  }
}

void vsync() {
  sync = sync ^ 1;
  ++vcnt;
  if (sync == 1) {
    pcnt = 0;
  }
}


void setup() {
  /*==============================================================================
 * Use timer1 to generate an 8 Mhz XCLK signal on pin 11
 *============================================================================*/

  // use timer1 to generate an 8 Mhz clock on pin 11
  cli();//disable interrupts
  /* Setup the 8mhz PWM clock 
   * This will be on pin 11*/
  DDRB|=(1<<3);//pin 11
  ASSR &= ~(_BV(EXCLK) | _BV(AS2));
  TCCR2A=(1<<COM2A0)|(1<<WGM21)|(1<<WGM20);
  TCCR2B=(1<<WGM22)|(1<<CS20);
  OCR2A=0; //no pre-scaler (F_CPU)/(2*(X+1))

  sei();//enable interrupts 

  Serial.begin(115200);

  uint8_t*b4=buf1;
  for (int x = 0; x < sizeof(buf1) / sizeof(buf1[0]); x++) {
    *b4++ = 0;
  }

  
  delay(3000);
 
  DDRC&=~15;//low d0-d3 camera
  DDRD&=~252;//d7-d4 and interrupt pins

  const byte pcklPin = 2;
  const byte vsyncPin = 3;
  b7 = &buf1[0];
  last_b7 = b7;

  
  pinMode(pcklPin, INPUT_PULLUP);
  pinMode(vsyncPin, INPUT_PULLUP);
//  attachInterrupt(digitalPinToInterrupt(vsyncPin), vsync, FALLING);
  attachInterrupt(digitalPinToInterrupt(pcklPin), pclk, FALLING);

}

void loop() {
  int lcnt = 0;
  
  while(vcnt % 100 == 0) {
    Serial.print("vcnt ");
    Serial.print(vcnt);
    Serial.print(" lcnt");
    Serial.print(lcnt++);
    Serial.print(" bcnt ");
    Serial.print(bcnt);
    if (last_b7 == &buf2[0]) {
      Serial.print(" buf1 ");
    } else {
      Serial.print(" buf2 "); 
    }

  for (int x = 0; x < 2; x++) {
    if (last_b7 == &buf2[0]) {
      Serial.print(buf1[x], HEX);
    } else {
      Serial.print(buf2[x], HEX); 
    }
   }
    Serial.println();
  }
     

}

  
