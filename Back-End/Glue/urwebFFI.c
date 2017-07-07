#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <urweb.h>

#define DEFAULT_BUFFER_SIZE 1024
#define BE_SAFE 1024 // deal with null termination, etc.

char * buf;

// JIM SAYS I can do this with Unix sockets, which might be faster.






char * readerHelper(int size){
  int fd;
  //Where to store the pipe according to library
  char * myfifo = "/tmp/myfifo2";
  //Set to empty
  buf = (char *) uw_malloc(size + BE_SAFE);	
  strcpy(buf, "empty");
  // open, read, and display the message from FIFO 
  //Keep pipe open untill it gets a message
  while(0 == strcmp("empty", buf)){
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
  char * myfifo2 = "/tmp/myfifo";
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
char * reader(){
  char * messageSize = readerHelper(DEFAULT_BUFFER_SIZE);
  int size = atoi(messageSize);
  char response[]="sizeok";
  writerHelper(response);
  return readerHelper(size);
}
void writer(char *message){
  int message_size = strlen(message);
  char message_size_buffer[DEFAULT_BUFFER_SIZE];
  snprintf(message_size_buffer, DEFAULT_BUFFER_SIZE, "%d", message_size); // assuming the size of the message isn't much over 10^100.
  writerHelper(message_size_buffer);
  readerHelper(DEFAULT_BUFFER_SIZE);
  writerHelper(message);
}











// this is actually the writer to Idris.

uw_Basis_string uw_UrwebFFI_counter(uw_context ctx, uw_unit u) {
 //char message[] = "Sending message from Ur/Web to Idris";
 //return messageManager(message); 


  


  writer("Sending message from Ur/Web to Idris");
  return reader();







/*char message[]="Hello";
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
*/

}



// IGNORE THESE!!!!!!!

uw_Basis_string uw_UrwebFFI_hello(uw_context ctx, uw_unit u) {
  return "Hello";
}

uw_Basis_string uw_UrwebFFI_important(uw_context ctx, uw_Basis_string s) {
  uw_Basis_string s2 = uw_malloc(ctx, strlen(s)+2);

  sprintf(s2, "%s!", s);
  return s2;
}

// actually this might be being used right now. should fix that.
//static int counter;




