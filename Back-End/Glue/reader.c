#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include "reader.h"


#define MAX_BUF 1024

char *reader()
{
	
	
        int fd;
        //Where to store the pipe according to library
        char * myfifo = "/tmp/myfifo";


//This section sets the size of the message, for the reader to expect
 	//Set to empty
        char *sizeOfMessage;
        sizeOfMessage = (char *) malloc(1024);
        
        //Legacy code
        //char *buf;
        
        //Initialize the character array
	strcpy(sizeOfMessage, "empty");
	
	
        // open, read, and display the message from FIFO 
        //Keep pipe open untill it gets a message
        while(0 == strcmp("empty", sizeOfMessage))
        {
                //Check in FIFO folder for a open pipe
                fd = open(myfifo, O_RDONLY);

                //Read the pipe (location, message, size of message)
                read(fd, sizeOfMessage, MAX_BUF);
        }
        
 //This section takes the incomming message size and allocates that on the heap
        char *message;

        int sizeOfNewMessage = atoi(sizeOfMessage);
        
        message = (char *) malloc(sizeOfNewMessage);
        //Initialize the character array
	strcpy(sizeOfMessage, "empty");
	
        while(0 == strcmp("empty", message))
        {
                //Check in FIFO folder for a open pipe
                fd = open(myfifo, O_RDONLY);

                //Read the pipe (location, message, size of message)
                read(fd, message, MAX_BUF);
        }
        
        //Print the message
        printf("Received: %s\n", sizeOfMessage);
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
        
        //Fill in size of message later where 128 is (messageSize)
        write(fd, message, sizeof(message));

        //Close the pipe
        close(fd);

        //remove the FIFO 
        unlink(myfifo);
}


/*
int main()
{

//reader();
 //char message[]="hello back";

//writer(message);


	return 0;
}
*/
