/*
    Damon Gwinn
    Flags for turning on different compiler functionalities
*/

#ifndef FLAGS_H
#define FLAGS_H

extern int use_cparser; /* Flag to use cparser instead of flex/bison */

void set_nonlocal_flag();
void set_o1_flag();
void set_o2_flag();

int nonlocal_flag();
int optimize_flag();

#endif
