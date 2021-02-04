static void captureImg(uint16_t wg,uint16_t hg){
  uint16_t lg2;
  uint8_t buf[640];
  Serial.println("RDY");
  //Wait for vsync it is on pin 3 (counting from 0) portD
  while(!(PIND&8));//wait for high
  while((PIND&8));//wait for low
  Serial.println("VS ");
    /*We send half of the line while reading then half later */
  while(hg--){
    uint8_t*b=buf,*b2=buf;
    lg2=wg/2;
    //Serial.print("---line--- ");
    //Serial.println(hg);    
    while(lg2--){
      while((PIND&4));//wait for low
      *b++=(PINC&15)|(PIND&240);
      Serial.print(*b, BIN);
      Serial.print(*b, BIN);  
      while(!(PIND&4));//wait for high
      while((PIND&4));//wait for low
      *b++=(PINC&15)|(PIND&240);
      Serial.print(*b, BIN);      
      //UDR0=*b2++;
      while(!(PIND&4));//wait for high
    }
    /* Finish sending the remainder during blanking */
    lg2=wg/2;
    while(lg2--){
      Serial.println(*b2++ , BIN);
    }
  }
}

uint16_t vcnt = 0;
uint16_t pcnt = 0;
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
  // reset pixel clock while vsync is high
  buf1[1]=2;
  if (PIND&8) {
    pcnt = 0;
    if (last_b7 == &buf1[0]) {
      b7 = &buf2[0];
    } else {
      b7 = &buf1[0];
    }
    last_b7 = b7;   
  }
  *b7 =(PINC&15)|(PIND&240);
  //flag = flag ^ 1;
  //data = (PINC&15)|(PIND&240);
  //valid = PIND&4; // when low
  //++pcnt;
  //++*b;
  if ((b7 - last_b7) < sizeof(buf1)) {
    ++b7; 
  } else {
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

  uint8_t*b6;   // a pointer to an unsigned byte type
  b6 = &buf1[0];   // b6 contains the address of buf

  uint8_t*b5;     // a pointer to an unsigned byte type
  b5 = &buf1[100]; // b5 contains the address of buf[100]

 /*
  for (int x = 0; x < 270; x++) {
    Serial.print( "x ");
    Serial.print(x);
    Serial.print( " b6 (addr) ");
    Serial.print((long) b6);
    Serial.print( " *b6 (contents) ");
    Serial.print( *b6 );
    *b6 = 2;
    Serial.print( " new *b6 (contents) ");
    Serial.print( *b6 );
    *b6 = 2;
    Serial.print( " b6 > &buf[20] ");
    Serial.println( b6 > &buf[20] );
    ++b6;
  }


  Serial.print( "buf[10] ");
  Serial.println(buf[10]);
  Serial.print( "buf[126] ");
  Serial.println(buf[126]);
  Serial.print( "buf[128] ");
  Serial.println(buf[128]);
  Serial.print( "buf[639] ");
  Serial.println(buf[639]);  

  Serial.print( "*b4 ");
  Serial.println(*--b4);

  Serial.print( "sizeof(buf[0]) ");
  Serial.println(sizeof(buf[0]));
  Serial.print( "sizeof(buf) ");
  Serial.println(sizeof(buf));
  */
  
  delay(5000);
 
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
  
  uint8_t*b8;   // a pointer to an unsigned byte type
  b8 = &buf1[3];   // b6 contains the address of buf
  *b8 = 3;
}

void loop() {
  delay(3000);
  /*
  while(!(PIND&8));//wait for high
  ++vcnt;
  while((PIND&8));//wait for low
     
  Serial.println("VS ");
  Serial.print( "vsync ");
 
  Serial.print( sync, BIN);
  Serial.print( " vcnt ");
  Serial.print( vcnt, DEC);
  Serial.print( " pcnt ");
  Serial.print( pcnt, DEC);
  Serial.print( " *b ");
  Serial.print( (long)&b, DEC);  
  Serial.print( " *b2 ");
  Serial.print( (long)&b2, DEC);  
  Serial.print( " *b-*b2 ");
  Serial.println( &b-&b2, DEC);  
  */
  if (last_b7 == &buf2[0]) {
      Serial.print("buf1 ");
    } else {
      Serial.print("buf2 "); 
    }

  for (int x = 0; x < sizeof(buf1) / sizeof(buf1[0]); x++) {
    if (last_b7 == &buf2[0]) {
      Serial.print(buf1[x], HEX);
    } else {
      Serial.print(buf2[x], HEX); 
    }
  }

    Serial.println();
  //captureImg(640,480);
  //while((PIND&4));//wait for low
/*  while(flag == 0);
  Serial.print( "vsync ");
  Serial.print( sync, BIN);
  Serial.print( " flag ");
  Serial.print( flag, BIN);
  Serial.print( " data ");
  Serial.print( data, BIN);
  Serial.print( " valid ");
  Serial.println( valid, BIN); 
  while(flag == 1);
  //while(!(PIND&4));//wait for high
  Serial.print( "vsync ");
  Serial.print( sync, BIN);
  Serial.print( " flag ");
  Serial.print( flag, BIN);
  Serial.print( " data ");
  Serial.print( data, BIN);
  Serial.print( " valid ");
  Serial.println( valid, BIN); 
  */

}

  
