#include "clock.h"
#include <stdio.h>

int set_tod_from_ports(tod_t *tod){

    //if time is out of bounds
    if (CLOCK_TIME_PORT < 0 || CLOCK_TIME_PORT > 86400 * 16){
        return 1;
    }

    //calculating secs
    //overtime to increment secs if needed
    int overtime = CLOCK_TIME_PORT % 16;
    //shift operation to divide by 16
    int secs = (CLOCK_TIME_PORT - overtime) >> 4;
    // if overtime is >= 9, then increment seconds
    if (overtime >= 9){
        secs++;
    }


    tod->day_secs = secs;

    //variable to calculate am/pm
    int ampm = 1;

    //calculating hours
    int hours = secs/3600;

    //case for 12 AM
    if (hours == 12){
        ampm = 2;
    }

    if (hours == 0){
        hours = 12;
    }

    if (hours > 12){
        hours = hours%12;
        ampm = 2;
    }
    
    //calculating time_mins
    int mins = secs/60;
    //overflow minutes! mod it
    if (mins > 60){
        mins = mins % 60;
    }

    //setting time_secs and ampm
    tod->time_mins = mins;

    tod->time_hours = hours;

    tod->time_secs = secs % 60;

    tod->ampm = ampm;

    return 0;
}

int set_display_from_tod(tod_t tod, int *display){

    //checking for all out of bounds possibilities
    if (tod.ampm < 0 || tod.day_secs < 0 || tod.time_hours < 0 || tod.time_mins < 0 || tod.time_secs < 0 || 
        tod.time_hours > 12 || tod.time_mins > 59 || tod.time_secs > 59 || (tod.ampm != 1 && tod.ampm != 2)){
        return 1;
    }

    //initialize display
    *display = 0;

    //create bitmask array
    int bitmasks[] = {0b1110111, //0
                      0b0100100, //1
                      0b1011101, //2
                      0b1101101, //3
                      0b0101110, //4
                      0b1101011, //5
                      0b1111011, //6
                      0b0100101, //7
                      0b1111111, //8
                      0b1101111, //9
                      0b0000000, //blank
                      0b0001000, //negative
                      0b1011011, //letter E
                      0b0111111, //letter R
    };

   // am pm setting
    if (tod.ampm == 1){ // am case
        *display = *display | (1 << 28);
    }
    else{ // pm case
        *display = *display | (1 << 29);
    }

    //first digit of hours
    //case for if two digits
    if (tod.time_hours > 9){
        *display = *display | (bitmasks[1] << 21);
        *display = *display | (bitmasks[tod.time_hours % 10] << 14);
    } else {
        //case for only one digit
        *display = *display | (bitmasks[tod.time_hours] << 14);
    }

    //first digit of minute
    //case for if two digits
    if (tod.time_mins > 9){
        *display = *display | (bitmasks[(int)tod.time_mins/10] << 7);
        *display = *display | (bitmasks[tod.time_mins % 10] << 0);
    } else {
        //case for only one digit
        *display = *display | (bitmasks[0] << 7);
        *display = *display | (bitmasks[tod.time_mins] << 0);
    }

    return 0;
}

int clock_update(){
    //today struct
    tod_t today;

    //error checking for first function
    if (set_tod_from_ports(&today) == 1){
        return 1;
    }

    //integer for in case set_display_from_tod gives error, don't want to edit 
    //CLOCK_DISPLAY_PORT in that case
    int temp = 0;

    //error check
    if (set_display_from_tod(today, &temp) == 1){
        return 1;
    }

    //if nothing went wrong, we set it to temp
    CLOCK_DISPLAY_PORT = temp;
    return 0;

}
