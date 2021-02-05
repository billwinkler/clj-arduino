
unsigned long lastTriggerTime;
volatile unsigned long triggerTime;
volatile boolean triggered;
uint8_t measurements = 0;

volatile boolean write_to_1;

uint16_t vcnt = 0;
uint16_t pcnt = 0;
volatile uint16_t bcnt = 0;

unsigned long t0;
unsigned long dt;

volatile byte sync = 0;

uint8_t buf1[128];
uint8_t buf2[128];

uint8_t*bp;       // a pointer to an unsigned byte type
uint8_t*curr_bp;  // last buffer points to the indicator

void timer () 
{
  // wait until we noticed last one
  if (triggered)
    return;

  triggerTime = micros ();
  triggered = true;
}  // end of timer

void vsync () 
{
  if (triggered)
    return;

  triggerTime = micros ();
  triggered = true;
  bcnt = 0;
  write_to_1 = true;
}  // end of vsync

void pclk () 
{
  if (!triggered)
    return;

  // write the data to the buffer
  *bp = (PINC&15)|(PIND&240);
  
  if ((bp - curr_bp) < sizeof(buf1)) {
    ++bp; 
  } else {
    // increment buffer counter
    ++bcnt;
    write_to_1 = !write_to_1;    
    if (write_to_1) {
      bp = &buf1[0];
    } else {
      bp = &buf2[0];
    } 
  }

  if (bcnt > 100) {
    triggered = false;
  }
}  // end of pclk

void vsync1() {
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

void pclk1() {
  // write the data to the buffer
  *bp = (PINC&15)|(PIND&240);

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
//  attachInterrupt(digitalPinToInterrupt(vsyncPin), timer, FALLING);
//  attachInterrupt(digitalPinToInterrupt(pcklPin), timer, FALLING);

}

void pr_pindc() {
  byte pins_d = PIND;
  byte pins_c = PINC;
  Serial.print( (pins_d >> 7) & 1 );
  Serial.print( (pins_d >> 6) & 1 );
  Serial.print( (pins_d >> 5) & 1 );
  Serial.print( (pins_d >> 4) & 1 ); 
  Serial.print( (pins_d >> 3) & 1 );
  Serial.print( (pins_d >> 2) & 1 );      
  Serial.print( (pins_d >> 1) & 1 );      
  Serial.print( pins_d & 1 );
  Serial.print( " " );            
  Serial.print( (pins_c >> 7) & 1 );
  Serial.print( (pins_c >> 6) & 1 );
  Serial.print( (pins_c >> 5) & 1 );
  Serial.print( (pins_c >> 4) & 1 ); 
  Serial.print( (pins_c >> 3) & 1 );
  Serial.print( (pins_c >> 2) & 1 );      
  Serial.print( (pins_c >> 1) & 1 );      
  Serial.println( pins_c & 1 );  
  } //end pr_pindc

void pr_data(byte data) {
  Serial.print( (data >> 7) & 1 );
  Serial.print( (data >> 6) & 1 );
  Serial.print( (data >> 5) & 1 );
  Serial.print( (data >> 4) & 1 ); 
  Serial.print( (data >> 3) & 1 );
  Serial.print( (data >> 2) & 1 );      
  Serial.print( (data >> 1) & 1 );      
  Serial.println(data & 1 );    
  } //end pr_data

void loop() {
    if (measurements > 10) 
    return;
    
    if (!triggered)
    return;

    //detachInterrupt(digitalPinToInterrupt(3));
    //Serial.println("triggered");  
    //while(!(PIND&8));//wait for vs high
    //while(!(PIND & B10000000));//wait for D7 high 
//    while(!(PIND & B00001000));//wait for vs high 
//    while(!(PIND & B00000100));//wait for pckl high 
    //while(!(PIND & B00010000));//wait for D4 high 
    
   unsigned long elapsed = micros() - triggerTime;
   if (elapsed < 3000L)
    {
    return;  // wait 3 milliseconds.
    }

  ++measurements;  

  if (write_to_1) {
    for (int x = 0; x < 10; x++) {
    pr_data(buf2[x]);   
    } 
  } else {
    for (int x = 0; x < 10; x++) {
    pr_data(buf1[x]);      
    }
  }
     

 /*   

  unsigned long elapsed = micros() - triggerTime;
  ++measurements;
 
  if (elapsed < 3000L)
    {
    triggered = false;
    return;  // ignore if less than 3 milliseconds.
    }
    
  triggered = false;  // re-arm for next time
  //attachInterrupt(digitalPinToInterrupt(3), timer, FALLING);
  
  Serial.print ("Took: ");
  Serial.print (elapsed);
  Serial.print (" microseconds. ");
  
  unsigned long minutes, seconds, ms;
  
  minutes = elapsed / (1000000L * 60);
  elapsed -= minutes * (1000000L * 60);
  
  seconds = elapsed / 1000000L;
  elapsed -= seconds * 1000000L;
  
  ms = elapsed / 1000;
  elapsed -= ms * 1000;
  
  Serial.print (minutes);
  Serial.print ("m ");
  Serial.print (seconds);
  Serial.print ("s ");
  Serial.print (ms);
  Serial.println ("ms.");

  for (int x = 0; x < 10; x++) {
      Serial.print(micros() - triggerTime);
      Serial.print(" ");
      pr_data((PINC&15)|(PIND&240));      
  }
  */

  /*
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
  */
    
}

  
