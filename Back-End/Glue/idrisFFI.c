#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include "idrisFFI.h"





#define DEFAULT_BUFFER_SIZE 1024
#define BE_SAFE 1024 // deal with null termination, etc.


int allocations;
int net_allocations;




void init(){
 allocations = 0;
 net_allocations = 0;
}

char * buf;

char *readerHelper(int size)
{
        int fd;

        //Where to store the pipe according to library
        char * myfifo = "/tmp/myfifo";


 	//Set to empty
        if(net_allocations > 0){
           free(buf);
           --net_allocations;
        }
	buf = (char *) malloc(size + BE_SAFE);
	++net_allocations;
        ++allocations;
	
	strcpy(buf, "empty");
        // open, read, and display the message from FIFO 

        //Keep pipe open untill it gets a message
        while(0 == strcmp("empty", buf))
        {
                //Check in FIFO folder for a open pipe
                fd = open(myfifo, O_RDONLY);

                //Read the pipe (location, message, size of message)
                read(fd, buf, size + BE_SAFE);
        }
        close(fd);
	return buf;
}

void writerHelper(char *message){
  int fd;
  char * myfifo2 = "/tmp/myfifo2";
  //Makes the FIFO nameed pipe
  mkfifo(myfifo2, 0666);
  //write to the FIFO pipe
  fd = open(myfifo2, O_WRONLY);
  //Actualy thing send (where im sending, message, size of message)
  write(fd, message, strlen(message));
  //Close the pipe
  close(fd);
  //remove the FIFO 
  unlink(myfifo2);
}




void writer(char *message){
  int message_size = strlen(message);
  char message_size_buffer[DEFAULT_BUFFER_SIZE];
  snprintf(message_size_buffer, DEFAULT_BUFFER_SIZE, "%d", message_size); // assuming the size of the message isn't much over 10^100.
  writerHelper(message_size_buffer);
  readerHelper(DEFAULT_BUFFER_SIZE);
  writerHelper(message);

  //printf("Number of allocations : %d \n", allocations);
  //printf("Total unfreed memory buffers : %d \n", net_allocations);
}


char * reader(){
  char * messageSize = readerHelper(DEFAULT_BUFFER_SIZE);
  int size = atoi(messageSize);
  char response[]="sizeok";
  writerHelper(response);
  return readerHelper(size);
}




int getRandom(){
// Eventually make this random, so that players do not predictably start as playerA or playerB.
// For now, randomness is not implemented (but this will still work)

    return 0;


}




