.text                           # IMPORTANT: subsequent stuff is executable
.global  set_tod_from_ports
        
set_tod_from_ports:

.start_point:
        movl    CLOCK_TIME_PORT(%rip), %ecx     # import clock_time_port into %ecx
        cmpl    $1382400, %ecx                  # check if clock_time_port is out of bounds (over 16 * seconds in a day)
        jg      .out_of_bounds                  # jump to out of bounds error handling section

        cmpl    $0, %ecx                        # also check if clock_time_port is less than 0
        jl      .out_of_bounds                  # go to out of bounds

        # dividend/divisor = quotient + remainder
        # eax / pass in = go into eax + edx

        movl    %ecx, %eax                      # move %ecx into %eax for division                     
        cqto                                    # prep for division
        movl    $16, %r8d                       # move 16 into r8d to be the divisor
        idivl   %r8d                            # divide eax by r8d, edx contains overtime
        subl    %edx, %ecx                      # clock_time_port - overtime
        movl    %eax, %ecx                      # move day_secs into ecx to save it and leave eax open for division
        cmpl    $9, %edx                        # check if overtime is over 9
        jl      .skip_incrementing_secs         # if overtime < 9, skip the incrementing seconds
        addl    $1, %ecx                        # else if overtime > 9, incremenet seconds

.skip_incrementing_secs:
        movl    %ecx, 0(%rdi)                   # move day_secs into the first slot of struct (0 to 4 bytes)
        movb    $1, %sil                        # set default of ampm to 1 (AM), store in sil

        #hours calculations!!
        movl    %ecx, %eax                      # move day_secs into eax to prep for division
        cqto                                    # prep for division
        movl    $3600, %r8d                     # move 60 * 60 (seconds in an hour) into r8d to be divisor
        idivl   %r8d                            # day_secs (eax) / 3600 (r8d) = hours (eax), remainder (edx)
        movl    %eax, %r10d                     # store hours into r10d
        cmpl    $12, %r10d                      # compare hours with 12 (check for 12 pm case)
        je      .12_PM_case                     # go to 12 pm case!

.after_12_pm_case:
        testl   %r10d, %r10d                    # after 12 pm case, see if hours is 0 (12 am case!)
        je      .12_AM_case                     # then go to 12 am case

.after_12_am_case:
        cmpl    $12, %r10d                      # check 12 with r10d!
        jg      .afternoon_case                 # if hours > 12, then it is the afternoon and go to afternoon case

.after_afternoon_case:
        #calculating minutes
        movl    %ecx, %eax                      # move day_secs to eax to prep for division
        cqto                                    # prep for division
        movl    $60, %r8d                       # move 60 (seconds in a minute) to r8d to be the divisor
        idivl   %r8d                            # day_secs (eax) / 60 (seconds in a minute) = minutes in an hour (eax), remainder (edx)
        movl    %eax, %r11d                     # eax is the minutes, move to r11d to save it
        cmpl    $60, %r11d                      # compare minutes with 60 (max minutes in a day)
        jg      .too_large_minutes_case         # if minutes > 60, something wrong, go to too large minutes case

.setting_vars:
        #calculating time_secs
        movl    %ecx, %eax                      # move day_secs into eax for division
        cqto                                    # prep for division
        movl    $60, %r8d                       # divide by 60 (seconds in a minute)
        idivl   %r8d                            # edx contains the remainder (time_secs)

        movl    %edx, 4(%rdi)                   # insert time_secs into struct at 4 to 6 bytes
        movl    %r11d, 6(%rdi)                  # insert minutes into struct at 6 to 8 bytes
        movl    %r10d, 8(%rdi)                  # insert hours into struct at 8 to 10 bytes
        movb    %sil, 10(%rdi)                  # insert ampm into struct at last 10 to 12 bytes

.returning:
        movl    $0, %eax                        # move 0 into eax to indicate success
        ret                                     # return!

.12_PM_case:
    movb    $2, %sil                            # move 2 into sil (ampm register) to indicate 12 pm
    jmp     .after_12_pm_case                   # move on to next case

.12_AM_case:
    movl    $12, %r10d                          # move 12 in r10d (hours register) when hours = 0 to indicate 12 am
    jmp     .after_12_am_case                   # move on to next case

.afternoon_case:
    movl    %r10d, %eax                         # move hours into eax to prep for division
    cqto                                        # prep for division
    movl    $12, %r8d                           # move 12 into r8d (12 hours in a day), we want the remainder in edx
    idivl   %r8d                                # hours (eax) / 12 (r8d) = 1 (eax), remainder/afternoon hours (rdx)
    movq    %rdx, %r10                          # adjust hours so that the remainder/true afternoon hour value is in r10 (hours register)
    movb    $2, %sil                            # move 2 into sil (ampm register) to indicate pm
    jmp .after_afternoon_case                   # go to after afternoon case

.too_large_minutes_case:
    movl    %r11d, %eax                         # move r11d (minutes register) into eax for division
    cqto                                        # prep for division
    movl    $60, %r8d                           # move 60 into r8d to divide (60 mins is the max)
    idivl   %r8d                                # minutes (eax) / 60 (r8d) = whatever (eax), remainder/minute overflow (edx)
    movl    %edx, %r11d                         # set minutes to our new actual minute value
    jmp     .setting_vars                       # go to setting packed struct section!

.out_of_bounds:
        movl    $1, %eax                        # move 1 into eax to indicate failure/error
        ret                                     # return 1!
                                         

.data                                           # data section for the array                 

bit_masks:                                      # bit mask array for the display numbers        
        .int 0b1110111  #0   
        .int 0b0100100  #1    
        .int 0b1011101  #2       
        .int 0b1101101  #3
        .int 0b0101110  #4
        .int 0b1101011  #5
        .int 0b1111011  #6
        .int 0b0100101  #7
        .int 0b1111111  #8
        .int 0b1101111  #9
        .int 0b0000000  #blank
        .int 0b0001000  #negative
        .int 0b1011011  #letter E
        .int 0b0111111  #letter R

.text
.global  set_display_from_tod
#                                       Bits	        Shift	 
#C Field Access	        Register	in reg	        Required	Size

#tod.day_secs	        %rdi	        0-31	        None	        4 bytes
#tod.time_secs	        %rdi	        32-47	        Right by 32     2 bytes
#tod.time_mins	        %rdi	        48-63	        Right by 48     2 bytes
#tod.time_hours	        %rsi	        0-15	        None            2 bytes
#tod.ampm	        %rsi	        16-23	        Right by 16     1 bytes

set_display_from_tod:
        #rdi - days_secs, time_secs, time_mins
        #rsi - time_hours, ampm
        #rdx - display int
        #r10d - bit_masks array pointer
        #r8 - use to create the 29 bit number

        movq    %rdx, %r10                      # move display pointer into r10 because we need rdx for division
        leaq    bit_masks(%rip),%r9             # load pointer to array into r9
        movq    $0, %r8                         # initialize 0 into r8 (temp display number)

#HANDLING AM PM 
.load_am_pm:
        movq    %rsi, %rcx                      # copy struct to rcx
        sarq    $16, %rcx                       # cut off until only ampm bits
        andq    $0xFF, %rcx                     # and with eight 1's to extract number, ecx contains ampm


        cmpl    $2, %ecx                        # compare ecx (ampm register) with 2
        je      .pm_case                        # if equal, go to pm case
        jg      .error_displaying               # if ecx (ampm) > 2, something wrong and go to error register

        cmpl    $1, %ecx                        # isn't pm, so compare ecx (ampm) with 1, don't jump bc no change needed
        jl      .error_displaying               # if somehow less than, go to error register
.am_case:
        orq    $0x10000000, %r8                 # or with 28 one's to initialize the int of 28 bits since am (one zero in front that gets cut off), r8 contains the number
        jmp     .load_hours                     # go to load hours!
.pm_case:
        orq    $0x20000000, %r8                 # or with 29 one's to initialize the int of 29 bits since pm case

#HANDLING HOURS
.load_hours:
        movq    %rsi, %rcx                      #copy rsi (containing hours and ampm) into rcx
        andq    $0xFFFF, %rcx                   # and with 16 one's, don't need to cut anything off since first 16 bits is hours, ecx contains hours now!
        cmpl    $12, %ecx                       # compare 12 (max hours in a day) with hours (ecx)
        jg      .error_displaying               # if somehow hours > 12, go to error handling
        cmpl    $9, %ecx                        # compare hours (ecx) with 9 to see if we need to go to the one digit or two digit hour case
        jg      .two_digit_hours                # if ecx > 9, go to two digit case

.one_digit_hours:                               # one digit case, just need to fill in second hour slot
        movl    (%r9, %rcx, 4), %r11d           # set index equal to the hour number (rcx) and extract bitmask (r9) value into r11d, 4 to indicate integer
        sall    $14, %r11d                      # pad r11d (bitmask number) with 14 bits to get to the right location
        orq     %r11, %r8                       # or r11 (bitmask number) with r8 (full number) to put bitmask in second hour slot
        jmp     .load_minutes                   # go to minute case

.two_digit_hours:                               # uh oh... we have two digits to worry about for hours
        #find the second digit!
        movl    %ecx, %eax                      # copy ecx (hours) into eax for division
        cqto                                    # prep for division
        movl    $10, %esi                       # move 10 into esi to be the divisor
        idivl   %esi                            # hours (eax) / 10 (esi) = 1 (eax), remainder/second digit of hours (edx), we want edx!

        #but first... need to fill that first space w a one!
        movq    $1, %rax                        # need 1 for index to pull from bitmask
        movl    (%r9, %rax, 4), %r11d           # get bitmask (r9) value from index 1 (rax) into r11d
        sall    $21, %r11d                      # pad w/ 21 0's to be in first hour slot
        orl     %r11d, %r8d                     # or with the display number (r8d) to put bitmask in 

        #now fill the second space!
        movl    (%r9, %rdx, 4), %r11d           # rdx is second digit of hours, get that index from bitmask (r9) and put into r11d
        sall    $14, %r11d                      # pad bitmask number (r11d) with 14 to put into second hour slot
        orl     %r11d, %r8d                     # or with display number (r8d) to put in right spot

#HANDLING MINUTES
.load_minutes:                                  # now for minutes!
        movq    %rdi, %rcx                      # copy rdi (contains day_secs, time_secs, time_minutes) into rcx
        sarq    $48, %rcx                       # cut off 48 bits from rcx to isolate minutes
        andq    $0xFFFF, %rcx                   # and with 16 one's to get the minutes!

        cmpl    $59, %ecx                       # compare minutes (ecx) with 59 
        jg      .error_displaying               # if minutes > 59, something is off, go to error handling

        cmpl    $9, %ecx                        # compare minutes (ecx) with 9 
        jg      .two_digit_mins                 # if minutes > 9, two digit minute case and need to handle that 
.one_digit_mins:                                # one digit minute case, very simple!
        #first... fill first digit with a 0!
        movq    $0, %rax                        # move 0 into rax to serve as bitmask array index
        movl    (%r9, %rax, 4), %r11d           # get 0th element (rax) from bitmask array (r9) and move into r11d
        sall    $7, %r11d                       # pad r11d with 7 zeros to be inserted in right spot in display int
        orl     %r11d, %r8d                     # or bitmask number (r11d) with display number (r8d) to put in right spot

        #now fill the second digit w the minutes!
        movl    (%r9, %rcx, 4), %r11d           # then use minutes (rcx) as index, and move into r11d!
        orl     %r11d, %r8d                     # can directly or since it's the last 7 bits
        jmp     .returning_update               # finish up by going to returning label
        
.two_digit_mins:                                # case if more than one digit... oh no
        #first... find the first digit!
        movl    %ecx, %eax                      # copy ecx (minutes) into eax
        cqto                                    # prep for division
        movl    $10, %esi                       # move 10 into esi to be divisor
        idivl   %esi                            # divide minutes (eax) by 10 (esi) = first minute digit (eax), second minute digit (edx)

        #fill in first digit!
        movl    (%r9, %rax, 4), %r11d           # get index (rax) of first minute digit and move into r11d
        sall    $7, %r11d                       # pad with 7 bits after to insert into first minute digit slot
        orl     %r11d, %r8d                     # or with display number (r8d) to put bitmask in 

        #fill in second digit!
        movl    (%r9, %rdx, 4), %r11d           # get index (rdx) of second minute digit and move into r11d
        orl     %r11d, %r8d                     # don't need to pad since last 7 bits, just or to put in last 7 bits

#RETURNING
.returning_update:
        movl    %r8d, (%r10)                    # put display number (r8d) into memory address pointed to by r10/input
        movl    $0, %eax                        # move 0 into eax to indicate success
        ret                                     # return!!

.error_displaying:
        movl    $1, %eax                        # move 1 into eax to indicate failure
        ret                                     # return!


.text
.global clock_update
        
clock_update:
        subq   $24, %rsp                        # make space for packed struct and align with 16 (24 + 16 = 32 % 16 = 0)    
        movq    %rsp, %rdi                      # copy stack pointer (rsp) to rdi to be an input to set_tod_from_ports function
        call    set_tod_from_ports              # can call function now, since stack aligned, return value in eax
                                                # stack is now full of packed struct from prev function call

        cmpl    $1, %eax                        # compare eax with 1 to see errors
        je      .error_handling                 # if eax == 1, do error handling
        
        movq    (%rsp), %rdi                    # put first 8 bits of stack (days_secs, time_secs, time_mins) into rdi to be used as inputs 
        movq    8(%rsp), %rsi                   # put last 4 bits of stack (time_hours, ampm) into rsi to be used as inputs
        leaq    CLOCK_DISPLAY_PORT(%rip), %rdx  # put clock_display_port pointer into rdx for input 

        call    set_display_from_tod            # now call set_display_from_tod with the inputs, return value in eax
        
        cmpl    $1, %eax                        # compare 1 with eax 
        jg      .error_handling                 # if eax == 1, do error handling 

        addq    $24, %rsp                       # restore the stack pointer to its original value
        
        movl    $0, %eax                        # move 0 into eax to indicate success
        ret                                     # return! finally over

.error_handling:
        addq    $24, %rsp                       # restore stack pointer in error case too!
        movl    $1, %eax                        # move 1 into eax to indicate failure
        ret                                     # return
