
uint16_t vcnt = 0;
uint16_t pcnt = 0;
uint16_t bcnt = 0;

unsigned long t0;
unsigned long dt;

volatile byte sync = 0;

uint8_t buf1[128];
uint8_t buf2[128];

uint8_t*bp;       // a pointer to an unsigned byte type
uint8_t*curr_bp;  // last buffer points to the indicator

void vsync() {
  sync ^= 1;
  ++vcnt;
  bcnt = 0;
  if (curr_bp == &buf1[0]) {
      bp = &buf2[0];
   } else {
      bp = &buf1[0];
   }
   curr_bp = bp;     
}

void pclk() {
  // write the data to the buffer
  *bp =(PINC&15)|(PIND&240);

  if ((bp - curr_bp) < sizeof(buf1)) {
    ++bp; 
  } else {
    // increment buffer counter
    ++bcnt;    
    if (curr_bp == &buf1[0]) {
      bp = &buf2[0];
    } else {
      bp = &buf1[0];
    }
    curr_bp = bp; 
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
  bp = &buf1[0];
  curr_bp = bp;

  
  pinMode(pcklPin, INPUT_PULLUP);
  pinMode(vsyncPin, INPUT_PULLUP);
  attachInterrupt(digitalPinToInterrupt(vsyncPin), vsync, FALLING);
  attachInterrupt(digitalPinToInterrupt(pcklPin), pclk, FALLING);

}

void loop() {
  int lcnt = 0;

  uint8_t*bp2,*bp3;
  
  while(vcnt % 100 == 0) {
    Serial.print("vcnt ");
    Serial.print(vcnt);
    Serial.print(" lcnt");
    Serial.print(lcnt++);
    Serial.print(" bcnt ");
    Serial.print(bcnt);
    if (curr_bp == &buf2[0]) {
      bp2 = &buf1[0];
      bp3 = (bp2+4);
      *bp3 = 0xFF;
      buf2[5] = 0xFF;
      Serial.print(" buf1 ");
    } else {
      bp2 = &buf2[0];
      bp3 = (bp2+4);
      *bp3 = 0xFF;
      buf1[5] = 0xFF;      
      Serial.print(" buf2 "); 
    }

 for (int x = 0; x < 10; x++) {
    if (curr_bp == &buf2[0]) {
      Serial.print(buf1[x], HEX);
      Serial.print(" ");      
    } else {
      Serial.print(buf2[x], HEX);
      Serial.print(" ");
    }
   }   

 
    Serial.println();
  }
    
}

  
