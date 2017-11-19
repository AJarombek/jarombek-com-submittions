// Author: Andrew Jarombek
// Date: 11/14/2017
// Sort a list of enums in C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int compare(const void *a, const void *b);

typedef struct {
    char first[20];
    char last[20];
} person;

int main() {
    person andrew;
    strcpy(andrew.first, "Andrew");
    strcpy(andrew.last, "Jar.");
    
    person thomas;
    strcpy(thomas.first, "Thomas");
    strcpy(thomas.last, "Cau.");
    
    person joe;
    strcpy(joe.first, "Joe");
    strcpy(joe.last, "Smi.");
    
    person ben;
    strcpy(ben.first, "Ben");
    strcpy(ben.last, "Fis.");
 
person people[] = {andrew, thomas, joe, ben};
 
// Get the length of the people array
int size = sizeof(people) / sizeof(people[0]);
 
for (int i = 0; i < size; i++) {
    printf("%d - %s %s \n", i + 1, people[i].first, people[i].last);
}
printf("\n");
 
qsort(people, size, sizeof(people[0]), compare);
 
for (int i = 0; i < size; i++) {
    printf("%d - %s %s \n", i + 1, people[i].first, people[i].last);
}
}

static int compare(const void *a, const void *b) {
    person *pA = (person *) a;
    person *pB = (person *) b;
    
    return strcmp(pA->last, pB->last);
}