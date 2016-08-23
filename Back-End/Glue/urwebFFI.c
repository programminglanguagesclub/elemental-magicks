#include <string.h>
#include <stdio.h>
#include <urweb.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_BUF 1024

int fd;




char * reader()
 {

     char * myfifo = "/temp/myfifo";

     //This section sets the size of the message, for the reader to expect
     //Set to empty
         char *sizeOfMessage;
         sizeOfMessage = (char *) malloc(128);
 
     //Initialize the character array
         strcpy(sizeOfMessage, "empty");
 
 
     // open, read, and display the message from FIFO 
     //Keep pipe open untill it gets a message
         while(0 == strcmp("empty", sizeOfMessage))
         {
                 //Check in FIFO folder for a open pipe
                 fd = open(myfifo, O_RDONLY);
 
                 //Read the pipe (location, message, size of message)
                 read(fd, sizeOfMessage, 128);
         }
     //For the first message might want something to check there is not garbage in the pipe
 
 
 
     //This section takes the incomming message size and allocates that on the heap
 
     //Character array to hold the message
     char *message;
 
     sizeOfMessage++; //Move pointer over one to ignore where it came from letter
 
     //Turn string into a int
     int sizeOfNewMessage = atoi(sizeOfMessage);
 
     //Allocate memery for incomming message
     message = (char *) malloc(sizeOfNewMessage);
 
     //Initialize the message
     strcpy(message, "empty");
 
 /*        while(0 == strcmp("empty", message))
         {
                 //Check in FIFO folder for a open pipe
                 fd = open(myfifo, O_RDONLY);
 
                 //Read the pipe (location, message, size of message)
                 read(fd, message, sizeOfNewMessage);
         }
         
         //Print the message
         printf("Received: %s\n", sizeOfMessage);
         printf("Received: %s\n", message);
 */
 
 
         close(fd);
         //unlink(myfifo);
 
         return message;
 }

void  writer(char *message)
{

    char * myfifo = "/temp/myfifo";
    //Open FIFO pipe
    fd = open(myfifo, O_WRONLY);

	//Actualy thing send (where im sending, message, size of message)
    //This was used for testing, default message with no size first sent
    //write(fd, message, sizeof(message));

    //Size of the message
    char * sizeOfMessage;
    itoa(message,sizeOfMessage,10);
    write (fd, message, 128);
    
	
    //Close the pipe
    close(fd);

    //Wait for response
    
     char *reply = (char *) malloc(128);
     strcpy(reply, "empty");

     while(0 == strcmp("empty", reply[0]) || 0== strcmp( "u",reply[0]))
    {         
        fd = open(myfifo, O_RDONLY);
        read(fd, sizeOfMessage, 128);

    }
     close(fd);


    //Send second message
    fd = open(myfifo, O_WRONLY);
    write(fd,message,sizeof(message)+4);

    close(fd);
    
}


uw_Basis_string uw_UrwebFFI_hello(uw_context ctx, uw_unit u) {
  return "Hello";
}

uw_Basis_string uw_UrwebFFI_important(uw_context ctx, uw_Basis_string s) {
  uw_Basis_string s2 = uw_malloc(ctx, strlen(s)+2);

  sprintf(s2, "%s!", s);
  return s2;
}

static int counter;

uw_Basis_string uw_UrwebFFI_counter(uw_context ctx, uw_unit u) {
 char message[]="Hello";
 char *buf;
 writer(message);
 //buf = (char *) malloc(1024); // memory leak
 buf = reader();
 //printf("About to return counter: %s\n", buf);
 
 //uw_Basis_string s2 = uw_malloc(ctx, 6+1);
 
 
 
 
 
 // REMOVE FOR NOW
 //return buf;//(&buf);
 

 return "testing literal string";








// return "Hello";
}































