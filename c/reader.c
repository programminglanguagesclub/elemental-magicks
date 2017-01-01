#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include "reader.h"


#define DEFAULT_BUFFER_SIZE 1024
#define BE_SAFE 1024 // deal with null termination, etc.

char*buf;
void freeMe()
{
	free(buf);
}


char *readerHelper(int size)
{
        int fd;

        //Where to store the pipe according to library
        char * myfifo = "/tmp/myfifo";


 	//Set to empty
 
	buf = (char *) malloc(size + BE_SAFE);
	
	
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
        //Print the message
         printf("Received: %s\n", buf);

        close(fd);
	return buf;
}

void writerHelper(char *message){
  printf("Sending %s \n", message);
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
  freeMe();
 	writerHelper(message); // Not sure what to do with the memory for message at this point. If we are calling this from Idris can we assume that this is on the stack and doesn't need to be managed?
}


char * reader(){
	char * messageSize = readerHelper(DEFAULT_BUFFER_SIZE);
  int size = atoi(messageSize);
  printf("GOT SIZE: %d\n", size);
  freeMe();
	char response[]="sizeok";
	writerHelper(response);
  return readerHelper(size);
}


void messageManager(){
  char * foo = reader(); // Here when Idris calls reader, it NEEDS to use something like CFFI.Memory (http://www.idris-lang.org/docs/0.12/contrib_doc/docs/CFFI.Memory.html), or we will have a leak.
  writer("lalala. IDRIS IS GREAT! URWEB IS GREAT! YAY!!!!! WOOHOO");
}

int main()
{

int y=1000;
//int y= 1;
while(y>0)
{
messageManager();
y--;
}
	return 0;
}
