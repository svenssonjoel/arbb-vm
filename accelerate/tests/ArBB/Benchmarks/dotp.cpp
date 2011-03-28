
#include<stdio.h>
#include<stdlib.h>
#include<memory.h>

// Just a simple sanity check.

typedef float elt;

int main() {
   printf("Hello\n");
   const int size = 1000000;
   elt* in1 = (elt*)malloc(size * sizeof(float));
   elt* in2 = (elt*)malloc(size * sizeof(float));

   for(int i=0; i<1000000; i++) {
       in1[i] = (float)rand();
       in2[i] = (float)rand();
   }

   printf("Done generating data\n");

   float sum = 0.0;
   for(int i=0; i<1000000; i++) {
       sum += in1[i] * in2[i];
   }
   printf("Sum was: %f\n", sum);
}

