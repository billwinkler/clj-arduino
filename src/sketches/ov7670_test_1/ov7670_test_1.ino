// use timer1 to generate an 8 Mhz clock on pin 11
// https://www.arduino.cc/en/Reference/PortManipulation
// https://github.com/ComputerNerd/ov7670-no-ram-arduino-uno/blob/master/main.c

// use Saleae Logic analyzer to verify XCLC, VS, PXCL, and D0 signals

void setup() {
  
  cli();//disable interrupts
  /* Setup the 8mhz PWM clock 
   * This will be on pin 11*/
  DDRB|=(1<<3);//pin 11
  ASSR &= ~(_BV(EXCLK) | _BV(AS2));
  TCCR2A=(1<<COM2A0)|(1<<WGM21)|(1<<WGM20);
  TCCR2B=(1<<WGM22)|(1<<CS20);
  OCR2A=0; //no pre-scaler (F_CPU)/(2*(X+1))

  sei();//enable interrupts

}

void loop() {
  // put your main code here, to run repeatedly:

}
