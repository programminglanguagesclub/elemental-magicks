#include <string.h>
#include <stdio.h>
#include <string.h>

#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#define DEFAULT_BUFFER_SIZE 1024
#define BE_SAFE 1024 // deal with null termination, etc.
char*buf;


//const int DEFAULT_BUFFER_SIZE = 1024;
void freeMe()
{
	free(buf);
}

char *readerHelper(int bytes)
{
 	int fd;
  
  //Where to store the pipe according to library
  char * myfifo2 = "/tmp/myfifo2"; 
	/*if(flag == 1){
		//Set to empty
		buf = (char *) malloc(MAX_BUF);
  }
	else{*/
  buf = (char*) malloc(bytes + BE_SAFE);	
	//}

	strcpy(buf,"empty");
  // open, read, and display the message from FIFO 
	//Keep pipe open until it gets a message
	while(0 == strcmp("empty", buf)){
		//Check in FIFO folder for a open pipe
    fd = open(myfifo2, O_RDONLY);
		
		//Read the pipe (location, message, size of message)
    read(fd, buf, bytes + BE_SAFE);
	}
	//Print the message
	printf("Received: %s\n", buf);
  close(fd);
	return buf;
}

void writerHelper(char *message){
	printf("Sending %s \n", message);
	int fd;
  char * myfifo = "/tmp/myfifo";
	//Makes the FIFO nameed pipe
  mkfifo(myfifo, 0666);
  //write to the FIFO pipe
  fd = open(myfifo, O_WRONLY);
	//Actualy thing send (where im sending, message, size of message)
  write(fd, message, strlen(message));
	//Close the pipe
  close(fd);
  //remove the FIFO 
  unlink(myfifo);
}

char * writer(char* input){


//Send outgoing message- size of message

  int message_size = strlen(input) /*+ BE_SAFE*/;
  char message_size_buffer[DEFAULT_BUFFER_SIZE];
  snprintf(message_size_buffer, DEFAULT_BUFFER_SIZE, "%d", message_size);


writerHelper(message_size_buffer);

//Read incomming message - size ok
char *sizeOK = readerHelper(DEFAULT_BUFFER_SIZE);

//Free the sizeOK message
freeMe();

//Send outgoing message -actual Message
writerHelper(input);

//Read incomming message - size of incomming message
char *sizeOfMessage = readerHelper(DEFAULT_BUFFER_SIZE);

//Save size of incomming message message
int size = atoi(sizeOfMessage);

printf("GOT SIZE: %d\n", size);

//Free sizeOfMessage2
freeMe();

//Send outgoing message - sizeok
writerHelper("sizeok");

//Read incomming message - actual message
char * message2 = readerHelper(size);

// We do not have to free message2 here: We can use CFFI.Memory in Idris (this file is for urweb though... still have to figure out how to do it there, but probably should just let it leak since that's the Ur/Web way)
//freeMe();


return message2;
}



int main(){
  int y;
  y = 1000;
  //y=1;
  while(y > 0){
    writer("Hello there");
    y--;
  }
  return 0;
}








