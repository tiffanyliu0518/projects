
#include "chester.h"
#include "chester_parse.h"


// Prints out a message if the environment variable DEBUG is set;
// Try running as `DEBUG=1 ./some_program`
void Dprintf(const char* format, ...) {
  if(getenv("DEBUG") != NULL){
    va_list args;
    va_start (args, format);
    char fmt_buf[2048];
    snprintf(fmt_buf, 2048, "|DEBUG| %s", format);
    vfprintf(stderr, fmt_buf, args);
    va_end(args);
  }
}

// Splits `line` into tokens with pointers to each token stored in
// argv[] and argc_ptr set to the number of tokens found. This
// function is in the style of strtok() and destructively modifies
// `line`. A limited amount of "quoting" is supported to allow single-
// or double-quoted strings to be present. The function is useful for
// splitting lines into an argv[] / argc pair in preparation for an
// exec() call.  0 is returned on success while an error message is
// printed and 1 is returned if splitting fails due to problems with
// the string.
//
// EXAMPLES:
// char line[128] = "Hello world 'I feel good' today";
// char *set_argv[32];
// int set_argc;
// int ret = split_into_argv(line, set_argv, &set_argc);
// // set_argc: 4
// // set_argv[0]: Hello
// // set_argv[1]: world
// // set_argv[2]: I feel good
// // set_argv[3]: today
int split_into_argv(char *line, char *argv[], int *argc_ptr){
  int argc = 0;
  int in_token = 0;
  for(int i=0; line[i]!='\0'; i++){
    if(!in_token && isspace(line[i])){                 // skip spaces between tokens
      continue;
    }
    else if(in_token && (line[i]=='\'' || line[i]=='\"')){
      printf("ERROR: No support for mid-token quote at index %d\n",i);
      return 1;
    }
    else if(line[i]=='\'' || line[i]=='\"'){           // begin quoted token
      char start_quote_char = line[i];
      i++;                                             // skip first quote char
      argv[argc++] = line+i;                           // start of token
      for(;line[i] != start_quote_char;i++){
        if(line[i] == '\0'){
          printf("ERROR: unterminated quote in <%s>\n",line);
          return 1;
        }
      }
      line[i] = '\0';                                  // end quoted token
    }
    else if(in_token && !isspace(line[i])){            // still in a token, move ahead
      continue;
    }
    else if(!in_token && !isspace(line[i])){           // begin unquoted token 
      in_token = 1;
      argv[argc++] = line+i;
    }
    else if(in_token && isspace(line[i])){             // end unquoted token
      in_token = 0;
      line[i] = '\0';
    }
    else{
      printf("ERROR: internal parsing problem at string index %d\n",i);
      return 1;
    }
  }
  *argc_ptr = argc;
  argv[argc] = NULL;                                   // ensure NULL termination required by exec()
  return 0;
}

// Returns a string constant representing the test_state_t for
// easy printing
char *test_state_str(test_state_t state){
  switch(state){
    case TEST_NOT_RUN: return "TEST_NOT_RUN";
    case TEST_RUNNING: return "TEST_RUNNING";
    case TEST_PASSED:  return "TEST_PASSED";
    case TEST_FAILED:  return "TEST_FAILED";
    default:           return NULL;
  }
}

// Initialize fields of `suite` to default values.
void suite_init(suite_t *suite){
  suite->infile = NULL;
  suite->infile_name = NULL;
  suite->prefix = NULL;
  suite->testdir = NULL;
  memset(suite->tests, 0, sizeof(test_t)*MAX_TESTS);
  suite->tests_count = 0;
  memset(suite->tests_torun, 0, sizeof(int)*MAX_TESTS);
  suite->tests_torun_count = 0;
  suite->tests_passed = 0;
  suite->max_procs = 0;
}

// Deallocate internal memory in the suite. All strings must be
// free()'d in the suite
void suite_dealloc(suite_t *suite){
  if(suite->infile_name != NULL) free(suite->infile_name);
  if(suite->prefix != NULL)      free(suite->prefix);
  if(suite->testdir != NULL)     free(suite->testdir);
  for(int i=0; i<suite->tests_count; i++){
    test_t *test = &suite->tests[i];
    if(test->title != NULL) free(test->title);
    if(test->description != NULL) free(test->description);
    if(test->program != NULL) free(test->program);
    if(test->input != NULL) free(test->input);
    if(test->infile_name != NULL) free(test->infile_name);
    if(test->output_expect != NULL) free(test->output_expect);
    if(test->output_actual != NULL) free(test->output_actual);
    if(test->outfile_name != NULL) free(test->outfile_name);
    if(test->resultfile_name != NULL) free(test->resultfile_name);
  }
}

// Initialize `suite` from a file using the instructor-provided PEG
// parser. The file `fname` is copied strdup()'d to the appropriate
// field as are various other fields.
int suite_init_from_file_peg(suite_t *suite, char *fname){
  suite_init(suite);
  FILE *infile = fopen(fname,"r");
  if(infile == NULL){
    printf("Unable to open file '%s'\n",fname);
    return -1;
  }
  suite->infile_name = strdup(fname);
  suite->infile = infile;
  
  chester_peg_context_t *ctx = chester_peg_create(suite);
  chester_peg_parse(ctx, NULL);
  chester_peg_destroy(ctx);
  fclose(suite->infile);
  suite->infile = NULL;
  if(suite->prefix == NULL){    // add default prefix if not set via directives
    suite->prefix = strdup("test");
  }
  if(suite->testdir == NULL){
    suite->testdir = strdup("chester-test");
  }
  return 0;
}

// Processes a global directive which changes attributes of the suite
// struct. This function is used during parsing for !key=val
// diretictives in the global directive section.
int suite_do_global_directive(suite_t *suite, const char *key, const char *val){
  if(0){}
  else if(strcmp(key,"prefix")==0){
    suite->prefix = strdup(val);
  }
  else if(strcmp(key,"testdir")==0){
    suite->testdir = strdup(val);
  }
  else{
    printf("ERROR: can't process global directive %s=%s\n",key,val);
    return 1;
  }
  return 0;
}


// Processes a local directive in the suite that changes attributes of
// the tests[] entry at index suite.tests_count.
int suite_do_local_directive(suite_t *suite, const char *key, const char *val){
  if(0){}
  else if(strcmp(key,"program")==0){
    suite->tests[suite->tests_count].program = strdup(val);
  }
  else if(strcmp(key,"exit_code")==0){
    suite->tests[suite->tests_count].exit_code_expect = atoi(val);
  }
  else{
    printf("ERROR: can't process local directive %s=%s\n",key,val);
    return 1;
  }
  return 0;
}
