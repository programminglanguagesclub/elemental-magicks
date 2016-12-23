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


char *reader(int type)
{
        int fd;

        //Where to store the pipe according to library
        char * myfifo = "/tmp/myfifo";


 	//Set to empty
        
	if(type==1)
	{
        	buf = (char *) malloc(12);
        }
	else
	{
		buf = (char *) malloc(type);
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

void writer(char *message)
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

void messageManager()
{
	//Read in message size
	char * messageSize = reader(1);
        freeMe();
	
	//Tell other I side got the message
	char response[]="sizeok";

	//Response i got the size of the message
	writer(response);

	//Get actualy Message message
	char * message = reader(atoi(messageSize));
	//freeMe();

	// Do whatever with message here
	//
	//
	//
	//	
	
	//printf("Message: %s", message);

	char * message2 = "Hello back";
	
	writer(message2);
	
	//Rolley wants freeme here
	freeMe();

}
/*
int main()
{

int y=10000;
while(y>0)
{
messageManager();
y--;
}
	return 0;
}
*/
