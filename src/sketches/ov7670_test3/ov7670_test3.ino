volatile unsigned long triggerTime;
volatile boolean triggered;
uint8_t measurements = 0;

volatile boolean write_to_1;
volatile uint16_t bcnt = 0;

uint8_t buf1[640];
uint8_t buf2[640];
volatile int idx = 0;

void pclk () 
{
  if (!triggered)
    return;
    
  // write the data to the active buffer
  if (write_to_1) {
    buf1[idx++] = (PINC&15)|(PIND&240);
  } else {
    buf2[idx++] = (PINC&15)|(PIND&240);
  }
  
  if (idx >= sizeof(buf1)) {
    idx = 0;
    // increment buffer counter
    ++bcnt;
    write_to_1 = !write_to_1; 
  }

  // 6200 buffers at 64 bytes per buffer
  
  if (bcnt > 6200) {
    triggered = false;
  }
}  // end of pclk

void pr_data(byte data) {
  Serial.print( (data >> 7) & 1 );
  Serial.print( (data >> 6) & 1 );
  Serial.print( (data >> 5) & 1 );
  Serial.print( (data >> 4) & 1 ); 
  Serial.print( (data >> 3) & 1 );
  Serial.print( (data >> 2) & 1 );      
  Serial.print( (data >> 1) & 1 );      
  Serial.print( data & 1 );
  Serial.print( " " );   
  } //end pr_data

  /* approximately 0.25 microseconds per pixel clock cycle
   * 0.16ms per row => 640 ticks 
   * 36 microsecond gap between rows => 144 ticks
   * 99 ms per vsync => 396000 ticks
   * 3.3 ms gap before first row => 13,200 ticks
   * 1.96 ms gap after last row => 7840 ticks
   * 480 rows, 784 ticks per row => 376320 ticks 
   * 376320 + 13200 + 7840 => 397360 ticks per frame, at 0.25 us => ~99.3ms per frame
   * 64 bytes per buffer => ~6200 buffers per frame
   * you can ignore the first 205
   */
void captureFrm () {
  while(!(PIND & B00001000));//wait for vs high
  idx = 0;
  bcnt = 0;
  write_to_1 = true;
  while((PIND & B00001000));//wait for vs low
  triggerTime = micros ();
  triggered = true;
    
  while(bcnt < 206); // wait for first data buffer
  while(bcnt < 300);
  // just testing
  uint16_t last_bcnt = bcnt;

  /*
  while(bcnt < 210) {
    Serial.print( bcnt );
    Serial.print( " " );
 */ 
//    while(bcnt == last_bcnt); // wait for next buffer
//    last_bcnt = bcnt; 
//  }
  
  for (int j = 0; j < 10; j++) {
  

  Serial.print( bcnt );


  //int limit = sizeof(buf1);
  int limit = 2;
  
  if (write_to_1) {
    Serial.print( " 2 " );
    for (int x = 0; x < limit; x++) {
    pr_data(buf2[x]);   
    } 
    Serial.println();
  } else {
    Serial.print( " 1 " );
    for (int x = 0; x < limit; x++) {
    pr_data(buf1[x]);      
    }
    Serial.println();
  }

  } // outer loop

  triggered = false;
} //end captureFrm

void setup() {
 /*==============================================================================
  * Use timer2 to generate an 1 Mhz XCLK signal on pin 11
  *============================================================================*/

  // see http://www.8bit-era.cz/arduino-timer-interrupts-calculator.html
  cli();//disable interrupts
  /* Setup the PWM clock 
   * This will be on pin 11*/
  DDRB|=(1<<3);//pin 11
  ASSR &= ~(_BV(EXCLK) | _BV(AS2));
  TCCR2A=(1<<COM2A0)|(1<<WGM21)|(1<<WGM20);
  TCCR2B=(1<<WGM22)|(1 << CS00);  
  OCR2A=7; // 1mhz 
//  OCR2A=0; //no pre-scaler (F_CPU)/(2*(X+1))

  sei();//enable interrupts 

  Serial.begin(115200); 
  delay(1000);
 
  DDRC&=~15;//low d0-d3 camera
  DDRD&=~252;//d7-d4 and interrupt pins

  const byte pcklPin = 2;
  pinMode(pcklPin, INPUT_PULLUP);
//  attachInterrupt(digitalPinToInterrupt(pcklPin), pclk, RISING);
}


void loop() {
  if (measurements > 10)
  return;    

  captureFrm();
   
  ++measurements; 
}

  
