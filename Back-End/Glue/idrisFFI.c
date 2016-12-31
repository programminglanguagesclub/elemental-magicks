#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include "idrisFFI.h"

#define MAX_BUF 1024



char*buf;
void freeMe()
{
	free(buf);
}


char *readerHelper(int flag)
{
        int fd;

        //Where to store the pipe according to library
        char * myfifo = "/tmp/myfifo";


 	//Set to empty
        
	if(flag==1)
	{
        buf = (char *) malloc(1024);
    }
	else
	{
		buf = (char *) malloc(1024);
	}
	
	strcpy(buf, "empty");
        // open, read, and display the message from FIFO 

        //Keep pipe open untill it gets a message
        while(0 == strcmp("empty", buf))
        {
                //Check in FIFO folder for a open pipe
                fd = open(myfifo, O_RDONLY);

                //Read the pipe (location, message, size of message)
                read(fd, buf, MAX_BUF);
        }
        //Print the message
         printf("Received: %s\n", buf);

        close(fd);
	return buf;
}

void writerHelper(char *message)
{
        printf("Sending %s \n", message);
int fd;
        char * myfifo2 = "/tmp/myfifo2";

        //Makes the FIFO nameed pipe
        mkfifo(myfifo2, 0666);

        //write to the FIFO pipe
        fd = open(myfifo2, O_WRONLY);

        //Actualy thing send (where im sending, message, size of message)
        write(fd, message, 128);

        //Close the pipe
        close(fd);

        //remove the FIFO 
        unlink(myfifo2);
}

char * reader()
{
	//Read incomming message -  size of incomming message
	char * messageSize = readerHelper(1);
    int size = 1024;
    //int size = strlen((messageSize);
    //size = size * 4
    
    //Free messageSize
    //freeMe();
	
	//Send outgoing message - size ok
	char response[]="sizeok";
	writer(response);

	//Read incomming message - actual message
    //printf("%d",size);
    char * message = readerHelper(size);
	
  return message;
}


void writer(char *message){
	
    //Do nothing with message

    //Free the message
    //freeMe()
    
    //Send outgoing message - size of message
    writer("12"); 

    //Read incomming message - sizeok
    char * messageOk = readerHelper(1024);


    //Free messageok
    //freeMe()

    //Send outgoing message - hello back
	char * message2 = "Hello back";
	writer(message2);
	

}



