#include "chester.h"

int main(int argc, char *argv[]){
// Defined in the file chester_main.c. Entry point for the Chester
// application which may be invoked with one of the following command
// line forms along with expected output.
//
// >> ./chester tests.md        # RUNS ALL TESTS
// tests.md : running 8 / 8 tests
// Running with single process: ........ Done
//  0) echo check           : FAIL -> see chester-test/prob1-result-00.txt
//  1) sleep 2s             : ok
//  2) pwd check            : FAIL -> see chester-test/prob1-result-02.txt
//  3) seq check            : ok
//  4) ls check             : FAIL -> see chester-test/prob1-result-04.txt
//  5) ls not there         : ok
//  6) wc 1 to 10           : FAIL -> see chester-test/prob1-result-06.txt
//  7) date runs            : ok
// Overall: 4 / 8 tests passed
//
// >> ./chester tests.md 2 4 6  # RUNS ONLY 3 TESTS NUMBERED 2 4 6
// tests.md : running 3 / 8 tests
// Running with single process: ... Done
//  2) pwd check            : FAIL -> see chester-test/prob1-result-02.txt
//  4) ls check             : FAIL -> see chester-test/prob1-result-04.txt
//  6) wc 1 to 10           : FAIL -> see chester-test/prob1-result-06.txt
// Overall: 0 / 3 tests passed
//
// main() parses the indicated input file to create a test suite
// struct. It then determines if all tests or only specified tests
// will be run by analyzing the command line argument structure. The
// `suite.tests_torun[]` and `suite.test_torun_count` fields are set
// according to which tests will be run: either specified tests only
// or 0..tests_count-1 for all tests. 
//
// Before running tests, output lines are printed indicating the test
// file and number of tests to be run versus the total number of tests
// in the file. The tests are then run and an output table is produced
// using appropriate functions. The "Overall" line is printed with the
// count of tests passed and that were actually run.
//
// >> ./chester -max_procs 4 tests.md         # RUN ALL TESTS WITH
// tests.md : running 8 / 8 tests             # 4 CONCURRENT PROCESSES
// Running with 4 processes: ........ Done
//  0) echo check           : FAIL -> see chester-test/prob1-result-00.txt
//  1) sleep 2s             : ok
//  2) pwd check            : FAIL -> see chester-test/prob1-result-02.txt
//  3) seq check            : ok
//  4) ls check             : FAIL -> see chester-test/prob1-result-04.txt
//  5) ls not there         : ok
//  6) wc 1 to 10           : FAIL -> see chester-test/prob1-result-06.txt
//  7) date runs            : ok
// Overall: 4 / 8 tests passed
//
// >> ./chester -max_procs 3 tests.md 2 4 6   # RUN 3 SELECTED TESTS WITH
// tests.md : running 3 / 8 tests             # 3 CONCURRENT PROCESSES
// Running with 3 processes: ... Done
//  2) pwd check            : FAIL -> see chester-test/prob1-result-02.txt
//  4) ls check             : FAIL -> see chester-test/prob1-result-04.txt
//  6) wc 1 to 10           : FAIL -> see chester-test/prob1-result-06.txt
// Overall: 0 / 3 tests passed
//
// Concurrently running processes are run via the associated
// `suite_run_tests_multiproc()` program.  The `-max_procs` command
// line flag sets the `suite.max_procs` field which is used in
// `suite_run_tests_multiproc()` to launch multiple processes to speed
// up test completion.

    // track infile_name from the 1 index of the argument array
    char *infile_name = argv[1];

    // create suite to be used for tests
    suite_t suite;

    // use helper function to initialize suite fields
    int ret = suite_init_from_file_peg(&suite, infile_name);

    // if suite fields couldn't be initialized 
    if(ret == -1){
        printf("something went wrong!!");
    }

    // case to run all tests/only two arguments passed in
    if (argc == 2){

        // set count of tests_torun equal to total tests
        suite.tests_torun_count = suite.tests_count;

        // loop thru all tests
        for (int testnum = 0; testnum < suite.tests_count; testnum++){

            // fill tests_torun array with all testnums
            suite.tests_torun[testnum] = testnum;
        }
    
    // case to run SOME tests/more than two arguments passed in
    } else {

        // count number of arguments 
        suite.tests_torun_count = argc - 2;

        // variable to loop over tests_torun indices
        int j = 0;

        // loop thru the number of extra arguments
        for (int i = 2; i < argc; i++){

            // put the extra arguments into the proper index in tests_torun
            suite.tests_torun[j] = atoi(argv[i]);

            //increment j
            j++;
        }
    }

    //print how many tests are being run with test_torun_count and total tests_count
    printf("%s : running %d / %d tests\n", infile_name, suite.tests_torun_count, suite.tests_count);

    // if something went wrong with running tests, do error handling
    if (suite_run_tests_singleproc(&suite) == -1){
        printf("ERROR: problems encountered during test run\n");
        exit(0);
    }

    // print the results
    suite_print_results_table(&suite);

    // print how many tests passed out of total
    printf("Overall: %d / %d tests passed\n", suite.tests_passed, suite.tests_torun_count);

    // use utility function to deallocate space
    suite_dealloc(&suite);    
}
