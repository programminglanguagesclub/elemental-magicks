#include <string.h>
#include <stdio.h>
#include <string.h>

#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#define MAX_BUF 1024
char*buf;

void freeMe()
{
	free(buf);
}

char *reader(int flag)
{
 	int fd;
	
	//Where to store the pipe according to library
        char * myfifo2 = "/tmp/myfifo2";
	if(flag == 1)
	{
		//Set to empty
		buf = (char *) malloc(1024); // memory leak
        }
	else
	{
		buf = (char*) malloc(flag);	
	}

	strcpy(buf,"empty");

	//char buf[MAX_BUF] = "empty";

        // open, read, and display the message from FIFO 
	//Keep pipe open untill it gets a message
	while(0 == strcmp("empty", buf))
	{
		//Check in FIFO folder for a open pipe
        	fd = open(myfifo2, O_RDONLY);
		
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
        char * myfifo = "/tmp/myfifo";

	//Makes the FIFO nameed pipe
        mkfifo(myfifo, 0666);

        //write to the FIFO pipe
        fd = open(myfifo, O_WRONLY);

	//Actualy thing send (where im sending, message, size of message)
        write(fd, message, 128);

	//Close the pipe
        	close(fd);

        //remove the FIFO 
        unlink(myfifo);
}

char * messageManager(char* input)
{

//Send outgoing message- size of message
char strSize[12];
sprintf(strSize, "%lu", sizeof(input));
writer(strSize);

//Read incomming message - size ok
char *sizeOK = reader(1);

//Free the sizeOK message
//freeMe()

//Send outgoing message -actual Message
writer(input);

//Read incomming message - size of incomming message
char *sizeOfMessage = reader(1);

//Save size of incomming message message
int size = 1024;
//size = strlen(sizeOfMessage);
//size = size*4;

//Free sizeOfMessage2
//freeMe()

//Send outgoing message - sizeok
writer("sizeok");

//Read incomming message - actual message
char * message2 = reader(size);

//Free message2
//freeMe();

return NULL; //returnMessage2;
}



int main()
{
int y;
y = 1000;
//y=1;
while(y >0)
{
messageManager("Hello there");

y--;
}
return 0;


}




/*
uw_Basis_string uw_Lib_hello(uw_context ctx, uw_unit u) {
  return "Hello";
}
uw_Basis_string uw_Lib_important(uw_context ctx, uw_Basis_string s) {
  uw_Basis_string s2 = uw_malloc(ctx, strlen(s)+2);
  sprintf(s2, "%s!", s);
  return s2;
}
static int counter;
uw_Basis_string uw_Lib_counter(uw_context ctx, uw_unit u) {
 char message[]="Hello";
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
}
*/


