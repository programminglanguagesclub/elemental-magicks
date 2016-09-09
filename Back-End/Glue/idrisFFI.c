#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include "idrisFFI.h"


#define MAX_BUF 1024
//int fd;
//Where to store the pip according to the library
//char * myfifo;

/*
void initializePipe()
{
    myfifo = "/tmp/myfifo/";
    mkfifo(myfifo, 0666);
}
*/

char * reader()
{
    perror("I started reader idiris");
int fd;
   int fd2;
    char * myfifo = "/tmp/myfifo";

    //This section sets the size of the message, for the reader to expect
 	//Set to empty
        char * sizeOfMessage = (char *) malloc(1024);
        
    //Initialize the character array
	    strcpy(sizeOfMessage, "empty");
	
	
    // open, read, and display the message from FIFO 
    //Keep pipe open untill it gets a message
        while(0 == strcmp("empty", sizeOfMessage))
        {
                //Check in FIFO folder for a open pipe
                fd = open(myfifo, O_RDONLY);
                //Read the pipe (location, message, size of message)
                read(fd, sizeOfMessage, 1024);
        }

        perror( " I finished reader idiris ");
   
        perror(sizeOfMessage);

        close(fd);
 
    //For the first message might want something to check there is not garbage in the pipe

    //This section takes the incomming message size and allocates that on the heap
   

    perror("I ran response idiris");
    //
    char * myfifo2 = "/tmp/myfifo2";
   // mkfifo(myfifo2, 0666);
    //printf("Sending %s \n", message);
   
    //Open FIFO pipe
    fd2 = open(myfifo2, O_WRONLY);
   
    //Actualy thing send (where im sending, message, size of message)
    write(fd2, "u got message", 1024);
   
   // write(fd, "testing" , 56);
    //Close the pipe
    //close(fd2);

    perror("Finished response idiris");
    
    close(fd);
/*

 	//Set to empty
        char * message = (char *) malloc(1024);
        
    //Initialize the character array
	    strcpy(message, "empty");
	

        perror(" Idiris start to get actual message");

    // open, read, and display the message from FIFO 
    //Keep pipe open untill it gets a message
        while(0 == strcmp("empty", message))
        {
                //Check in FIFO folder for a open pipe
                fd = open(myfifo, O_RDONLY);
                //Read the pipe (location, message, size of message)
                read(fd, message, 1024);
        }

        perror ("Idiris done " );

   // close(fd);



    //unlink(myfifo);


    //    return message;
   //unlink(myfifo);
   //perror("I ran reader");
  */  
    return sizeOfMessage;
}



void  writer(char *message)
{

    //perror("I ran writer");
    int fd;
    char * myfifo2 = "/tmp/myfifo2";
//    mkfifo(myfifo2, 0666);
    //printf("Sending %s \n", message);
   
    //Open FIFO pipe
    fd = open(myfifo2, O_WRONLY);
   
    //Actualy thing send (where im sending, message, size of message)
    write(fd, message, strlen(message));
   
   // write(fd, "testing" , 56);
    //Close the pipe
    close(fd);
  
    //remove the FIFO 
    unlink(myfifo2);
  
}


