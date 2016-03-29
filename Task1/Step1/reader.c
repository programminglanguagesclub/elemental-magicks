#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>

#define MAX_BUF 1024

void reader()
{
        int fd;

        //Where to store the pipe according to library
        char * myfifo = "/tmp/myfifo";

        //Set to empty
        char buf[MAX_BUF] = "empty";

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


int main()
{

reader();
 char message[]="hello back";

writer(message);


	return 0;
}
