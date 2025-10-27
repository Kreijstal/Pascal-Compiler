/*
    Damon Gwinn
    Flags for turning on different compiler functionalities
*/

#ifndef FLAGS_H
#define FLAGS_H

void set_nonlocal_flag();
void set_o1_flag();
void set_o2_flag();
void set_parse_only_flag();

int nonlocal_flag();
int optimize_flag();
int parse_only_flag();

#endif
