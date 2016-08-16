#include <string.h>
#include <stdio.h>
#include <urweb.h>

#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_BUF 1024


char *reader()
{
 	int fd;
	
	//Where to store the pipe according to library
        char * myfifo = "/tmp/myfifo";
	
	//Set to empty
	char *sizeOfIncommingMessage;
	sizeOfIncommingMessage = (char *) malloc(1024); // memory leak
	strcpy(sizeOfIncommingMessage, "empty");
        //char buf[MAX_BUF] = "empty";

        // open, read, and display the message from FIFO 
	//Keep pipe open untill it gets a message
	while(0 == strcmp("empty", sizeOfIncommingMessage))
	{
		//Check in FIFO folder for a open pipe
        	fd = open(myfifo, O_RDONLY);
		
		//Read the pipe (location, message, size of message)
        	read(fd, sizeOfIncommingMessage, MAX_BUF);
	}
	
	//Set to empty
	char *message;
	int sizeOfMessage = atoi(sizeOfIncommingMessage);
	message = (char *) malloc(sizeOfMessage); // memory leak
	strcpy(message, "empty");

        // open, read, and display the message from FIFO 
	//Keep pipe open untill it gets a message
	while(0 == strcmp("empty", message))
	{
		//Check in FIFO folder for a open pipe
        	fd = open(myfifo, O_RDONLY);
		
		//Read the pipe (location, message, size of message)
        	read(fd, message, sizeOfMessage);
	}
	//Print the message
	 printf("Received: %s\n", sizeOfIncommingMessage);

	//Print the message
	 printf("Received: %s\n", message);

        close(fd);
	return message;
}

void writer(char *message)
{
	printf("Sending %s \n", message);
int fd;
        char * myfifo = "/tmp/myfifo";

	//Makes the FIFO nameed pipe
        mkfifo(myfifo, 0666);

        //write to the FIFO pipe
        fd = open(myfifo, O_WRONLY);

	//Actualy thing send (where im sending, message, size of message)
        write(fd, message, 128);

	//Close the pipe
        close(fd);

        //remove the FIFO 
        unlink(myfifo);
}






uw_Basis_string uw_Lib_hello(uw_context ctx, uw_unit u) {
  return "Hello";
}

uw_Basis_string uw_Lib_important(uw_context ctx, uw_Basis_string s) {
  uw_Basis_string s2 = uw_malloc(ctx, strlen(s)+2);

  sprintf(s2, "%s!", s);
  return s2;
}

static int counter;

uw_Basis_string uw_Lib_counter(uw_context ctx, uw_unit u) {
 char message[]="Hello";
 writer(message);
 char *buf;
 //buf = (char *) malloc(1024); // memory leak
 buf = reader();
 //printf("About to return counter: %s\n", buf);
 //return counter++;
 
 //uw_Basis_string s2 = uw_malloc(ctx, 6+1);
 //sprintf(s2,"%s","hahaha");
 return buf;//(&buf);
 
// return "Hello";
}































