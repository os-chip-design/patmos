#include "include/bootable.h"
#include <machine/spm.h>

#define LOOP_DELAY 2000
#define N 1000


int main() 
{
	volatile _SPM int *led_ptr  = (volatile _SPM int *) PATMOS_IO_LED;
	volatile _UNCACHED static int arr[N];
	int k = 0;
	int i = 0;
	
	for(k= 0; k < N; k++)	{ arr[k] = k; }
	

	for(;;) 
	{	
		if(arr[i] == i) {
			*led_ptr = 5;
		}
		else {
			*led_ptr = 6;
		}

		i = (i + 1) % N;
	}
}
