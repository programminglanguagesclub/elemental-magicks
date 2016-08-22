#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include "reader.h"


#define MAX_BUF 1024
int fd;
//Where to store the pip according to the library
char * myfifo = "/tmp/myfifo/";



   char * writer(char *message)
   {
       //printf("Sending %s \n", message);
   
       //Makes the FIFO nameed pipe
       mkfifo(myfifo, 0666);
   
       //Open FIFO pipe
       fd = open(myfifo, O_WRONLY);
   
       //Actualy thing send (where im sending, message, size of message)
       write(fd, message, sizeof(message));
   
       //Close the pipe
       close(fd);
   
       //remove the FIFO 
       //unlink(myfifo);
   
       return message;
   
   }

void reader()
{
		
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

        //return message;
}

