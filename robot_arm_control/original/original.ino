//#include <Servo.h>
#include "VarSpeedServo.h"
#include <EEPROM.h> // memory whose values are kept when the  board is turned off (like a small hard drive)

// assigning potentiometers to analog pins since potentiometers give an analog output which will then be converted to a range of 0-1023

int pot0=A0; //base
int pot1=A1; //main
int pot2=A2; //forearm
int pot3=A3; //end effector

// using the VarspeedServo library to assign the servos to pins
// Class for manipulating servo motors connected to Arduino pins.


VarSpeedServo servo0;
VarSpeedServo servo1;
VarSpeedServo servo2;
VarSpeedServo servo3;


int val0;
int valmapped0;
int val1;
int valmapped1;
int val2;
int valmapped2;
int val3;
int valmapped3;


//servoSequencePoint loops(120A120B120C120D);

byte mode; // stores an 8 bit unassigned number from 0-255 since there are two modes



void setup()
{
 Serial.begin(9600);

 Serial.write("Let's begin");
 
 // attach(pin )  - Attaches a servo motor to an i/o pin.
 servo0.attach(5);
 servo1.attach(6);
 servo2.attach(9);
 servo3.attach(10);

 pinMode(pot0,INPUT); // not necessary since pins are input puins by default
}




void loop()
{ 
 // The static keyword is used to create variables that are visible to only one function. 
 //However unlike local variables that get created and destroyed every time a function is called,
 //static variables persist beyond the function call, preserving their data between function calls.
  
  
  static int t=0;

  if(Serial.available()){

    char mychar=Serial.read();

    //Like if statements, 
    //switch case controls the flow of programs by allowing programmers 
    //to specify different code that should be executed in various conditions.
    //When a case statement is found whose value matches that of the variable,
    //the code in that case statement is run.The break keyword exits the switch statement.

    switch(mychar){
      //mychar: a variable whose value to compare with various cases.
      
      case '0'...'9':
        t=t*10 + mychar - '0';

        break;

      case 'a':
        {
          if(mode==1)
            servo0.write(t,10); //  write(value, speed) - speed varies the speed of the move to new position 0=full speed, 1-255 slower to faster
        }
        t=0;

        break;

      case 'b':
        {
          if(mode==1)
            servo1.write(t,10);
        }
        t=0;

        break;

      case 'c' :
        {
          if(mode==1)
            servo2.write(t,10);
        }
        t=0;

        break;

      case 'd':
        {
          if(mode==1)
            servo3.write(t,10);  
        }
        t=0;

        break;


// trying to make the joints move sequentially, not all at the same time. Check the library for another servo.write deviant
// write(value, speed, wait) - wait is a boolean that, if true, causes the function call to block until move is complete
// Boolean is a value just like an integer that can take a number, but instead of 0-65535 it can take a number 0 or 1.


         case 'A':
        {
          if(mode==1)
            servo0.write(t,10,1); 
        }
        t=0;

        break;

      case 'B':
        {
          if(mode==1)
            servo1.write(t,10,1);
        }
        t=0;

        break;

      case 'C' :
        {
          if(mode==1)
            servo2.write(t,10,1);
        }
        t=0;

        break;

      case 'D':
        {
          if(mode==1)
            servo3.write(t,10,1);  
        }
        t=0;

        break;

        //case 'L':
         //{
          //if(mode==1)
          //  sequencePlay(loops,3,true,90);
        // }
        // t=0;

         //break;

      case 'm':
        {
          Serial.print("Servos will be controlled using potentiometers");
          delay(2000);
          mode=0;
          Serial.println(mode);
        }
        break;
        t=0;


      case 'p':
        {
          Serial.print("Servos will be controlled using angles  ");
          mode=1;
          Serial.print(mode);
        }
        break;
        t=0;

       case 'z':
        {
          Serial.print("Servos will be controlled using automatically  ");
          mode=2;
          Serial.print(mode);
        }
        break;
        t=0;

        

      case 'W':
    {
      delay(2000);
    }

    break;



      
    }





      
  }


  
if(mode==0){
    // for rotating base

    
    val0=analogRead(pot0); 
    valmapped0=map(val0,0,1023,0,179);//maps numbers to the servo range,0-180 degrees
    Serial.println(valmapped0); 
    servo0.write(valmapped0); // writes a value to a servo controlling the shaft accordingly
    delay(10);

// for rotating main arm


    val1=analogRead(pot1);
    valmapped1=map(val1,0,1023,0,179);
    Serial.println(valmapped1);
    servo1.write(valmapped1);
    delay(10);

    // for rotating forearm
    
    
    val2=analogRead(pot2); 
    valmapped2=map(val2,0,1023,0,179); //maps numbers to the servo range,0-180 degrees 
    Serial.println(valmapped2);
    servo2.write(valmapped2); // writes a value to a servo controlling the shaft accordingly
    delay(10);


    // for rotating end-effector
    
    val3=analogRead(pot3);
    valmapped3=map(val3,0,1023,0,179);
    Serial.println(valmapped3);
    servo3.write(valmapped3);
    delay(10);   
  }

/// automatic control


if(mode==2){
  servo0.write(45,20,1);
    delay(100);
    servo1.write(45,20,1);
    delay(100);
    servo2.write(45,20,1);
    delay(100);
    servo3.write(100,20,1);
    delay(100);


    servo0.write(90,20,1);
    delay(100);
    servo1.write(90,20,1);
    delay(100);
    servo2.write(90,20,1);
    delay(100);
    servo3.write(90,20,1);
    delay(100);


    servo0.write(120,10,1);
    delay(1000);
    servo1.write(120,10,1);
    delay(1000);
    servo2.write(120,10,1);
    delay(1000);
    servo3.write(120,10,1);
    delay(1000);


    servo0.write(150,10,1);
    delay(1000);
    servo1.write(150,10,1);
    delay(1000);
    servo2.write(150,10,1);
    delay(1000);
    servo3.write(180,10,1);
    delay(1000);
}
}
