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


char *itoa(long i, char* s, int dummy_radix) {
        sprintf(s, "%ld", i);
            return s;
}


void  writer(char *message)
{

    int fd;
    int fd2;

    char * myfifo = "/tmp/myfifo";
    mkfifo(myfifo,0666);
   char * myfifo2 = "/tmp/myfifo2";
   mkfifo(myfifo2, 0666);
    //Open FIFO pipe
    fd = open(myfifo, O_WRONLY);

	//Actualy thing send (where im sending, message, size of message)
    //This was used for testing, default message with no size first sent

    //Size of the message
    char * sizeOfMessage = (char *) malloc(strlen(message)+1);


    perror("Urweb sending message size");
   //snprintf(sizeOfMessage,"%d",sizeof(message) +4);
    strcpy(sizeOfMessage,message);
    write (fd, sizeOfMessage, sizeof(sizeOfMessage));
    perror("Urweb sent message size");
	
    //Close the pipe
    close(fd);
   

/////*
  //call reader.... once reader returns continue....
   
    //The error comes from te open FD 
    
//http://www.gnu.org/software/libc/manual/html_node/Creating-a-Pipe.html#Creating-a-Pipe
    char *reply = (char *) malloc(128);
     strcpy(reply, "empty");

     perror("ur web messaged reader " );
     
     long x = 10000000L;
     while(0 == strcmp("empty", reply))
    {         
        fd2 = open(myfifo2, O_RDONLY);
        read(fd2, reply, 1024);

        if(x-- < 0)
                {
                    strcpy(reply, "Failed getting reply");
                    break;
                }


    }
     perror("The message i got was: ");
     perror(reply);
     close(fd2);
    
/////*/
/*
    //Send message
    fd = open(myfifo, O_WRONLY);
    int i;

    perror("Sending message urweb " );
    write(fd,message, sscanf(sizeOfMessage, "%d", &i));

    perror("Message sent ur web");
  
    
    //close(fd);
    */
    unlink(myfifo);

}


char * reader()
 {
int fd;
     char * myfifo2 = "/tmp/myfifo2";

     //This section sets the size of the message, for the reader to expect
     //Set to empty
         char *sizeOfMessage;
         sizeOfMessage = (char *) malloc(128);
 
     //Initialize the character array
         strcpy(sizeOfMessage, "empty");
 

         long x = 1000000000L;
     // open, read, and display the message from FIFO 
     //Keep pipe open untill it gets a message
         while(0 == strcmp("empty", sizeOfMessage) )
         {
                 //Check in FIFO folder for a open pipe
                 fd = open(myfifo2, O_RDONLY);
 
                 //Read the pipe (location, message, size of message)
                read(fd, sizeOfMessage, 128);
                if(x-- < 0)
                {
                    strcpy(sizeOfMessage, "failed");
                    break;
                }
         }
     //For the first message might want something to check there is not garbage in the pipe
 
 
        close(fd);
        //unlink(myfifo2);
         return sizeOfMessage;
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
 return buf;//(&buf);
 

// return "testing literal string";

// return "Hello";
}




