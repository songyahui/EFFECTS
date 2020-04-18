//https://create.arduino.cc/projecthub/vicentezavala/iot-stepper-motor-2db08f?ref=tag&ref_id=control&offset=14
/*
*	Author		: Zavala Ortiz Vicente Arturo.
*	language	: .cpp
*	Created		: 3/11/2015 11:56:57 AM
*	Name		: ATmega32u4_WiFiStepperKnob.cpp
*	Update		: 1/9/2017 02:29:15 PM
*	Description : wifi Socket Client
*/

#include <util/delay.h>
#include <stdlib.h>

#include "CC3000.h"
#include "CC3000_Client.h"
#include "AFMotor.h"
#include "common.h"

/*****		wifi Global Variables		*****/
// Connection info data lengths
#define MAX_MSG_LEN							100
#define DHT11_PIN							0
// Constants

char ap_ssid[]				= "SSID of network";	    // SSID of network
char ap_password[]		= "Password of network";	// Password of network

unsigned int ap_security	= WLAN_SEC_WPA2;	  // Security of network
unsigned long timeout		  = 3000;            	// Milliseconds

// Global Variables
CC3000 wifi						= CC3000();
CC3000_Client client  = CC3000_Client(wifi);

char in_message[MAX_MSG_LEN]	= {0};

/*****		Other Global Variables		*****/
static char page[20], data[20];
signed int option, value, out, stepper2_actual_value = 0;

void setup()
{
	/* add setup code here */
 	ConnectionInfo connection_info;
 
 	// Initialize CC3000 (configure SPI communications) 	
 	Serial.begin(115200);
 	 
 	// Initialize CC3000 (configure SPI communications)
 	if(!wifi.init(9)) {
 			Serial.println("Initialize CC3000 FAIL!");
 		return;
 	}

	else {
		Serial.println"Initialize CC3000 OK");
	}

	if(!wifi.connect(ap_ssid, ap_security, ap_password, timeout)) {	
		Serial.println("Error: Could not connect to AP!");
	}

	else
		return;
	
	
	_delay_ms(5000);
	
	// Gather connection details and print IP address
	if(!wifi.getConnectionInfo(connection_info) )
	{
		Serial.println("Error: Could not obtain connection details");
		return;
	}
	
	else 
	{	
		Serial.println("IP Address: ");
		printIPAddr(connection_info.ip_address);
	}

	_delay_ms(5000);
}

uint8_t i;

void loop()
{		
	/* add main program code here */
 	Serial.println("WAITING FOR MESSAGE!");

	fulsh_buffer(in_message);

	if(client.readfrom(4444, in_message, MAX_MSG_LEN) == -1) {
		Serial.println("NO MESSAGE RECEIVED");
	}
	
	Serial.println(in_message);
			

  if(client.readfrom(4444, in_message, MAX_MSG_LEN) == -1) {
	  Serial.println("NO MESSAGE RECEIVED");
  }

	else
	{
	  stepper2.setSpeed(200);

		if(http_gets(data, "stepper_motor2", in_message)) {
		  value = atoi(data);
							
			if(value > 0) {
				  stepper2.step(value, FORWARD, SINGLE);
			}

			else if (value < 0) {
				  stepper2.step(value, BACKWARD, SINGLE);				
			}
	}
	
	fulsh_buffer(in_message);
}