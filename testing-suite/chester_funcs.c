// chester_funcs.c: Service functions for chester primarily operating
// upon suite_t structs.

#include "chester.h"

////////////////////////////////////////////////////////////////////////////////
// PROBLEM 1 Functions
////////////////////////////////////////////////////////////////////////////////

int suite_create_testdir(suite_t *suite){
// Creates the testing results directory according to the
// name in the suite `testdir` field. If testdir does not exist, it is
// created as directory with permisions of User=read/write/execute
// then returns 1. If testdir already exists and is a directory, does
// nothing and returns 0. If a non-directory file named testdir
// already exists, print an error message and return -1 to indicate
// testing cannot proceed. The error message is:
//
// ERROR: Could not create test directory 'XXX'
//        Non-directory file with that name already exists
//
// with XXX substituted with the value of testdir


    //sets the results_dir string to be the value in testdir
    char* results_dir = suite->testdir;

    //creates a stat struct for results_dir
    struct stat results_dir_stats;

    // case if results_dir already exists, and is a directory
    if (stat(results_dir, &results_dir_stats) == 0 && S_ISDIR(results_dir_stats.st_mode)){
        return 0;

    // case if results_dir already exists, and isn't a directory - error case
    } else if (stat(results_dir, &results_dir_stats) == 0){
        printf("ERROR: Could not create test directory '%s'\n", results_dir);
        printf("    Non-directory file with that name already exists\n");
        return -1;
    }

    // case if results_dir doesn't exist
    // S_IRUSR - user read, S_IWUSR - user write, S_IXUSR - user execute
    mode_t permissions = S_IRUSR | S_IWUSR | S_IXUSR;

    // make directory with the right permissions
    mkdir(results_dir, permissions);
    return 1;
}

// this function uses strdup() to create the name of the file string
char* suite_make_name(suite_t *suite, int testnum, char* inout){

    // set prefix and testdir strings equal to the testdir and prefix in suite struct
    char* testdir = suite->testdir;
    char* prefix = suite->prefix;

    // set a string to hold the outfile name
    char name[MAX_FILENAME];

    // case if the file is an input/output file, grouped together since they both have .txt
    if (strcmp(inout, "input") == 0 || strcmp(inout, "output") == 0){

        // assembles the file name using sprintf
        sprintf(name, "%s/%s-%s-%02d.txt", testdir, prefix, inout,testnum);

    // case if file is result file, has .md 
    } else if (strcmp(inout, "result") == 0){

        //assembles the file name using sprintf
        sprintf(name, "%s/%s-%s-%02d.md", testdir, prefix, inout,testnum);
    }

    // use strdup to duplicate name into the heap, use file_name to point to it
    char* file_name = strdup(name);

    return file_name;
}


int suite_test_set_outfile_name(suite_t *suite, int testnum){

    // set the string type to be passed into suite_make_name
    char* type = "output";

    // suite, testnum, and "output" passed into helper function suite_make_name
    // which assembles string and uses strdup
    char* outfile = suite_make_name(suite, testnum, type);

    // set suite outfile_name to outfile string
    suite->tests[testnum].outfile_name = outfile;

    return 0;
}

int suite_test_create_infile(suite_t *suite, int testnum){

    // error checking if the input is null, return
    if (suite->tests[testnum].input == NULL){
        return 0;
    }

    // string to store type to be passed into suite_make_name
    char* type = "input";

    // pass in inputs into helper function suite_make_name which assembles the 
    // file name and uses strdup()
    char* infile_name = suite_make_name(suite, testnum, type);

    //set the infile_name in suite to the infile_name string
    suite->tests[testnum].infile_name = infile_name;

    // open the infile with write/create/truncate, user read/write permissions
    int in_fd = open(infile_name, O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR); 

    // error in opening/creating the file
    if (in_fd == -1){
        perror("Could not create input file");
        return -1;
    }

    // write input to the infile_name file, write the number of characters in input
    write(in_fd, suite->tests[testnum].input, strlen(suite->tests[testnum].input));

    return 0;
}

int suite_test_read_output_actual(suite_t *suite, int testnum){
// Reads the contents of the file named in field
// `outfile_name` for the given testnum into heap-allocated space and
// assigns the output_actual field to that space. Uses a combination
// of stat() and read() to efficiently read in the entire contents of
// a file into a malloc()'d block of memory, null terminates it (\0)
// so that the contents may treatd as a valid C string. Returns the
// total number of bytes read from the file on on success (this is
// also the length of the `output_actual` string). If the file could
// not be opened or read, the `output_actual` field is not changed and
// -1 is returned.


    // set outfile string to suite outfile_name
    char* outfile = suite->tests[testnum].outfile_name;
    
    // create stat struct to store stats about outfile_name
    struct stat out_stats;

    // fill struct with stats about outfile
    stat(outfile, &out_stats);

    // try to open the outfile
    int out_fd = open(outfile, O_RDONLY);

    // error case with opening outfile, return -1
    if (out_fd == -1){
        perror("Couldn't open file");
        return -1;
    }

    // get the file size of the output file
    int file_size = out_stats.st_size;

    // malloc space for the output, file_size + 1 (for null terminator)
    char* output = malloc(file_size + 1);

    // read the stuff in the outfile into output
    int bytes_read = read(out_fd, output, file_size + 1);

    // error case, couldn't read the file, return -1
    if (bytes_read == -1){
        perror("Couldn't read file");
        return -1;
    }

    // manually insert null terminator at the end of output string
    output[bytes_read] = '\0';

    // set suite output to output string
    suite->tests[testnum].output_actual = output;

    return bytes_read;
}


int suite_test_start(suite_t *suite, int testnum){
// Start a child process that will run program in the
// indicated test number. The parent process first sets the
// outfile_name and creates infile_name with the program input. It
// then creates a child process, sets the test field `child_pid` to
// the child process ID and returns 0.
//
// The child sets up output redirection so that the standard out AND
// standard error streams for the child process is channeled into the
// file named in field `outfile_name`. Note that standard out and
// standard error are "merged" so that they both go to the same
// `outfile_name`. This file should have the same options used when
// opening it as described in suite_test_create_infile(). If
// infile_name is non-NULL, input redirection is also set up with
// input coming from the file named in field `infile_name`. Uses the
// split_into_argv() function to create an argv[] array which is
// passed to an exec()-family system call.
//
// Any errors in the child during input redirection setup, output
// redirection setup, or exec()'ing print error messages and cause an
// immediate exit() with an associated error code. These are as
// follows:
//
// | CONDITION            | EXIT WITH CODE         |
// |----------------------+------------------------|
// | Input redirect fail  | exit(TESTFAIL_INPUT);  |
// | Output redirect fail | exit(TESTFAIL_OUTPUT); |
// | Exec failure         | exit(TESTFAIL_EXEC);   |
//

    // set the oufile_name
    if (suite_test_set_outfile_name(suite, testnum) == -1){
        printf("Couldn't set outfile name\n");
        return -1;
    }

    // create/read the infile
    if (suite_test_create_infile(suite, testnum) == -1){
        printf("Couldn't read infile\n");
        return -1;
    }

    // set the test's state to currently running 
    suite->tests[testnum].state = TEST_RUNNING;

    // create child!!
    pid_t child = fork();

    // child code!!
    if (child == 0){

        // --- INPUT ---
        // check if the infile is not null/skip if input is null
        if (suite->tests[testnum].infile_name != NULL){

            // set infile_name string to the suite infile
            char* infile_name = suite->tests[testnum].infile_name;

            // open the infile
            int in_fd = open(infile_name, O_RDONLY);

            //error case for if can't open file
            if (in_fd < -1){
                printf("ERROR: Couldn't read file\n");
            }

            // use dup2 to redirect input, check for error and properly exit if so
            if (dup2(in_fd, STDIN_FILENO) == -1){
                exit(TESTFAIL_INPUT);
            }
        }
        // --- /INPUT ---

        // --- OUTPUT ---
        // get the outfile_name from the suite
        char* outfile_name = suite->tests[testnum].outfile_name;

        // open/create the outfile with permissions previously set
        int out_fd = open(outfile_name, O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR); 

        // redirect standard out and error out, do error checking and return properly if so
        if (dup2(out_fd, STDOUT_FILENO) == -1 || dup2(out_fd, STDERR_FILENO) == -1){
            exit(TESTFAIL_OUTPUT);
        }
        // --- /OUTPUT ---

        // --- EXEC ---
        // create array with max_args positions
        char* argv_array[MAX_ARGS];
        
        // variable to store the number of args
        int num_args;

        //use utility function to split the suite program into the array
        split_into_argv(suite->tests[testnum].program, argv_array, &num_args);

        // execvp with first program input and error checking, properly exit if so
        if (execvp(argv_array[0], argv_array) == -1){
            perror("ERROR: test program failed to exec");
            exit(TESTFAIL_EXEC);
        }
        // --- /EXEC ---
        
    }

    // parent ending code, set pid in suite 
    suite->tests[testnum].child_pid = child;

    return 0;
}

int suite_test_finish(suite_t *suite, int testnum, int status){
// Processes a tests after its child process has completed and
// determines whether the tests passes / fails.
//
// The `status` parameter comes from a wait()-style call and is use to
// set the `exit_code_actual` of the test. `exit_code_actual` is one
// of the following two possibilities:
// - 0 or positive integer: test program exited normally and the exit
//   code/status is stored.
// - Negative integer: The tested program exited abnormally due to
//   being signaled and the negative of the signal number is
//   stored. Ex: child received SIGSEGV=11 so exit_code_actual is -11.
// If `status` indicated neither a normal nor abnormal exit, this
// function prints an error and returns (this case is not tested).
//
// Output produced by the test is read into the `output_actual` field
// using previously written functions.
//
// The test's `state` field is set to one of TEST_PASSED or
// TEST_FAILED. Comparisons are done between the fields:
// - output_expect vs output_actual (strings)
// - exit_code_expect vs exit_code_actual (int)
//
// If there is a mismatch with these, the test has failed and its
// `state` is set to TEST_FAILED. If both sets of fields match, the
// state of the test becomes `TEST_PASSED` and the suite's
// `tests_passed` field is incremented.


    // create variable to hold the test status
    int test_status = 0;

    // if child ended abnormally 
    if (WIFSIGNALED(status)){

        // multiply -1 with wtermsig to get the proper negative return value
        test_status = (-1) * WTERMSIG(status);

    // if child ended normally
    } else if (WIFEXITED(status)) {

        //set test_status to be the exit status
        test_status = WEXITSTATUS(status);
    }

    // set actual exit code to the test status var
    suite->tests[testnum].exit_code_actual = test_status;

    // read in output into file
    suite_test_read_output_actual(suite, testnum);

    // var to keep track of output match and exit code match
    int checking_state = 0;

    // if expected output is null, then automatically add to checking_state (AKA skip output check)
    if (suite->tests[testnum].output_expect == NULL){
        checking_state++;
    }

    // if didn't skip output check/if expected output is not null, then compare actual and expected output
    // increment if so
    if (checking_state == 0 && strcmp(suite->tests[testnum].output_actual, suite->tests[testnum].output_expect) == 0){
        checking_state++;
    }

    // check if actual and expected exit codes match
    if (suite->tests[testnum].exit_code_actual == suite->tests[testnum].exit_code_expect){
        checking_state++;
    }

    // if both tests are passed/if checking state is 2
    if (checking_state == 2){

        // set test state to passed
        suite->tests[testnum].state = TEST_PASSED;

        // increment tests_passed in suite
        suite->tests_passed++;

    // else if failed output/exit code or both, failed test
    } else {

        // set test state to failed
        suite->tests[testnum].state = TEST_FAILED;
    }

    return 0;
}


void print_window(FILE *out, char *str, int center, int lrwidth){

// Print part of the string that contains index center to the given
// out file. Print characters in the string between
// [center-lrwidth,center+lrwidth] with the upper bound being
// inclusive. If either the start or stop point is out of bounds,
// truncate the printing: the minimum starting point is index 0, the
// maximum stopping point is the string length.
//
// EXAMPLES:
// char *s = "ABCDEFGHIJKL";
// //         012345678901
// print_window(stdout, s, 4, 3);
// // BCDEFGH
// // 1234567
// print_window(stdout, s, 2, 5);
// // ABCDEFGH
// // 01234567
// print_window(stdout, s, 8, 4);
// // EFGHIJKL
// // 45678901
//
// NOTE: this function is used when creating test results to show
// where expected and actual output differ

    // calculate upper and lower bounds of the string
    int upper_bound = center + lrwidth;
    int lower_bound = center - lrwidth;

    // if lower bound is out of bounds, set to 0
    if (lower_bound < 0){
        lower_bound = 0;
    }

    // if upper bounds is out of bounds, set to max len of string
    if (upper_bound > strlen(str)){
        upper_bound = strlen(str) - 1;
    }

    // get number of characters in the string to print
    int num_charas = upper_bound - lower_bound + 1;

    // print the proper number of characters that are in the string starting from the lower_bound
    fprintf(out, "%.*s\n", num_charas, &str[lower_bound]);
}

int differing_index(char *strA, char *strB){

// Finds the lowest index where different characters appear in strA and
// strB. If the strings are identical except that one is longer than
// the other, the index returned is the length of the shorter
// string. If the strings are identical, returns -1.
//
// EXAMPLES:
// differing_index("01234567","0123x567") -> 4
// differing_index("012345","01234567")   -> 6
// differing_index("012345","01x34567")   -> 2
// differing_index("012345","012345")     -> -1
// 
// NOTE: this function is used when creating test results to show
// where expected and actual output differ

    // find both lens of both strings
    int lenA = strlen(strA);
    int lenB = strlen(strB);

    // var to determine which length to loop over
    int len_use = lenA;

    // if string b is longer than string a, use string b length to loop over
    if (lenB > lenA){
        len_use = lenB;
    }

    // loop over the longer string
    for (int i = 0; i < len_use; i++){

        // if one character doesn't match, return that index
        if (strA[i] != strB[i]){

            // return index of mismatch
            return i;
        }
    }

    // something went wrong somewhere..
    return -1;
}

int suite_test_make_resultfile(suite_t *suite, int testnum){

// Creates a result file for the given test. The general format is shown in the example below.
//   # TEST 6: wc 1 to 10 (FAIL)              // testnum and test title, print "ok" for passed tests
//   ## DESCRIPTION
//   Checks that wc works with input          // description field of test
//
//   ## PROGRAM: wc                           // program field of test
//
//   ## INPUT:                                // input field of test, "INPUT: None" for NULL input
//   1
//   2
//   3
//   4
//   5
//   6
//   7
//   8
//   9
//   10
//                                            // if output_expect is NULL, print "OUTPUT: skipped check"
//   ## OUTPUT: MISMATCH at char position 3   // results of differing_index() between 
//   ### Expect                               // output_expect and output_actual fields
//   10 10 21                                 // output_expect via calls to print_window()
//
//   ### Actual
//   10  9 20                                 // output_actual via calls to print_window()
//                                            // if no MISMATCH in output, prints ## OUTPUT: ok
//
//   ## EXIT CODE: ok                         // MISMATCH if exit_code_expect and actual don't match and
//                                            // prints Expect/Actual values
//   ## RESULT: FAIL                          // "ok" for passed tests
//
// The file to create is named according to the pattern
//
// TESTDIR/PREFIX-result-05.md
//
// with TESTDIR / PREFIX and 05 substituted for the `testdir` and
// `prefix` fields of suite and 05 for the testnum (width 2 and
// 0-padded). Note the use of the .md extension to identify the output
// as Markdown formatted text.
//
// The output file starts with a heading which prints the a heading
// with the testnum and title in it along with ok/FAIL based on the
// `state` of the test. Then 6 sections are printed which are
// 1. DESCRIPTION
// 2. PROGRAM
// 3. INPUT
// 4. OUTPUT (comparing output_expect and output_actual)
// 5. EXIT CODE (comparing exit_code_expect and exit_code_actual)
// 6. RESULT
//
// In the OUTPUT section, if a difference is detected at position N
// via the differing_index() function, then a window around position N
// is printed into the file for both the expected and actual
// output. The window width used is defined in the header via the
// constant TEST_DIFFWIDTH and is passed to print_window() function.
//
// If the output_expect field is NULL, the OUTPUT section header has
// the message "skipped check" printed next to it.
//
// In the EXIT CODE section, if there is a mismatch between the
// expected and actual exit_code, then they are both printed as in:
// ## EXIT CODE: MISMATCH
// - Expect: 0
// - Actual: 1
//
// The final RESULT section prints either ok / FAIL depending on the
// test state.
//
// If the result file cannot be opened/created, this file prints the
// error message
//   ERROR: Could not create result file 'XXX'
// with XXX substituted for the file name and returns -1. Otherwise
// the function returns 0 on successfully creating the resultfile.
    
    // set type to be used in suite_make_name
    char* type = "result";

    // use helper function that assembles string name and uses strdup to make result file name
    char *result_name = suite_make_name(suite, testnum, type);

    // set suite resultfile_name field to result name
    suite->tests[testnum].resultfile_name = result_name;

    // open result_file with writing permissions
    FILE* result_file = fopen(result_name, "w");

    // if file is invalid, error case and return -1
    if (result_file == NULL){
        printf("ERROR: Could not create result file '%s'\n", result_name);
        return -1;
    }

    // --- TEST STATUS ---
    // set default test_status to ok
    char* test_status = "ok";

    // if test failed, set status to FAIL
    if (suite->tests[testnum].state == TEST_FAILED){
        test_status = "FAIL";
    }

    // print out test number, title, and proper status (ok/FAIL)
    fprintf(result_file, "# TEST %d: %s (%s)\n", testnum, suite->tests[testnum].title, test_status);
    // --- /TEST STATUS ---


    // --- DESCRIPTION ---
    fprintf(result_file, "## DESCRIPTION\n");
    
    // print out test description
    fprintf(result_file, "%s", suite->tests[testnum].description);
    // --- /DESCRIPTION ---


    // --- PROGRAM ---
    // printing out program 
    fprintf(result_file, "## PROGRAM: %s\n", suite->tests[testnum].program);
    // --- /PROGRAM ---


    // --- INPUT ---
    // if input is null/there is no input to print
    if (suite->tests[testnum].input == NULL){

        // print none for input 
        fprintf(result_file, "## INPUT: None\n");

    // else if input is not null/there exists an input to print
    } else {

        // print proper input
        fprintf(result_file, "## INPUT:\n%s\n", suite->tests[testnum].input);
    }
    // --- /INPUT ---

    // --- OUTPUT ---
    // if expected output is null/didn't check output
    if (suite->tests[testnum].output_expect == NULL){

        // print that output was skipped
        fprintf(result_file, "## OUTPUT: skipped check\n");
    }

    // else if expected output is not null/did check output
    else {

        // use differing_index function to find where the tests differed
        int diff = differing_index(suite->tests[testnum].output_expect, suite->tests[testnum].output_actual);

        // if expected and output strings were the same
        if (diff == -1){
            fprintf(result_file, "## OUTPUT: ok\n");

        // else if expected and output strings did not match
        } else {

            // print that there was a mismatch at the differing_index return index
            fprintf(result_file, "## OUTPUT: MISMATCH at char position %d\n", diff);
        
            fprintf(result_file, "### Expect\n");
            // use print_window to print the expected output 
            print_window(result_file, suite->tests[testnum].output_expect, diff, TEST_DIFFWIDTH);

            fprintf(result_file, "### Actual\n");
            // use print_window to print the actual output 
            print_window(result_file, suite->tests[testnum].output_actual, diff, TEST_DIFFWIDTH);
        }
    }
    // --- /OUTPUT ---


    // --- EXIT CODE ---
    // if expected and actual exit codes don't match
    if (suite->tests[testnum].exit_code_actual != suite->tests[testnum].exit_code_expect){

        // print that were was a mismatch
        fprintf(result_file, "## EXIT CODE: MISMATCH\n");

        //print expected and actual exit codes
        fprintf(result_file, "- Expect: %d\n", suite->tests[testnum].exit_code_expect);
        fprintf(result_file, "- Actual: %d\n", suite->tests[testnum].exit_code_actual);

    // else if expected and actual test codes match
    } else {

        // print that they were ok!
        fprintf(result_file, "## EXIT CODE: ok\n");
    }
    // --- /EXIT CODE ---

    // --- RESULT ---
    // if state of test is passed
    if (suite->tests[testnum].state == TEST_PASSED){

        // print ok!
        fprintf(result_file, "## RESULT: ok\n");

    // if test didn't pass, print fail
    } else {
        fprintf(result_file, "## RESULT: FAIL\n");
    }
    // --- /RESULT ---

    // close result_file scanner 
    fclose(result_file);

    return 0;
}


int suite_run_tests_singleproc(suite_t *suite){

// Runs tests in the suite one at time. Before begining the tests,
// creates the testing directory with a call to
// suite_create_testdir().  If the directory cannot be created, this
// function returns -1 without further action.
//
// The tests with indices in the field `tests_torun[]` are run in the
// order that they appear there. This is done in a loop.
// `suite_test_start(..)` is used to start tests and wait()-style
// system calls are used to suspend execution until the child process
// is finished. Additional functions previously written are then used
// to
// - Assign the exit_code for the child
// - Read the actual output into the test struct
// - Set the pass/fail state
// - Produce a results file for the test
//
// Prints the "Running with single process:" and each test that
// completes prints a "." on the screen to give an indication of
// progress. "Done" is printed when all tests complete so that a full
// line which runs 8 tests looks like
//
//    Running with single process: ........ Done
//
// If errors arise such as with waiting for a child process, failures
// with getting the test output, or other items, error messages should
// be printed but the loop should continue. No specific error messages
// are required and no testing is done; error messages are solely to
// aid with debugging problems.

    // create testdir and do error checking, return -1 if error creating dir
    if (suite_create_testdir(suite) == -1){
        printf("ERROR: Failed to create test directory\n");
        return -1;
    }

    // print for extra info
    printf("Running with single process: ");

    // loop over the tests that need to be run, not all tests
    for (int i = 0; i < suite->tests_torun_count; i++){

        // find the testnum to run from the tests_torun array
        int testnum = suite->tests_torun[i];

        // start the test and do error handling, return -1 if error
        if (suite_test_start(suite, testnum) == -1){
            printf("ERROR: Couldn't start test\n");
            return -1;
        }

        // int to keep track of status of child
        int status;

        // wait for the child, do error handling if something happened
        if (waitpid(suite->tests[testnum].child_pid, &status, 0) == -1){
            printf("ERROR: Problem with child\n");
        }

        // finish the test, give it status of child, do error handling
        if (suite_test_finish(suite, testnum, status) == -1){
            printf("ERROR: Couldn't finish test\n");
        }

        // create the result_file, do error handling
        if (suite_test_make_resultfile(suite, testnum) == -1){
            printf("ERROR: Couldn't make result file\n");
        }

        // print a dot for every test that was run
        printf(".");
    }

    // print done at the end!
    printf(" Done\n");
    return 0;
}

void suite_print_results_table(suite_t *suite){

// Prints a table of test results formatted like the following.
//
//  0) echo check           : FAIL -> see chester-test/prob1-result-00.txt
//  1) sleep 2s             : ok
//  2) pwd check            : FAIL -> see chester-test/prob1-result-02.txt
//  3) seq check            : ok
//  4) ls check             : FAIL -> see chester-test/prob1-result-04.txt
//  5) ls not there         : ok
//  6) wc 1 to 10           : FAIL -> see chester-test/prob1-result-06.txt
//  7) date runs            : ok
//
// The test number at the beginning of the line is printed with width
// 2 and space padded. The Test title is printed with a width of 20,
// left-aligned using capabilities of printf().  If the test passes,
// the message "ok" is added while if it fails, a FAIL appears and the
// result file associated with the test is indicated. This function
// honors the `tests_torun[]` array and will only print table results
// for tests with indices in this array.

    // loop over the tests that need to be run, not all tests
    for (int i = 0; i < suite->tests_torun_count; i++){

        // get the testnum of the current test
        int testnum = suite->tests_torun[i];

        // if test was a fail
        if (suite->tests[testnum].state == TEST_FAILED){

            // print proper string with FAIL
            printf("%2d) %-20s : FAIL -> see %s\n",
                testnum, suite->tests[testnum].title, suite->tests[testnum].resultfile_name);

        // else if test passed
        } else {

            // print proper string with ok
            printf("%2d) %-20s : ok\n", testnum, suite->tests[testnum].title);
        }

    }

}
