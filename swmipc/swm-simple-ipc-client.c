#include "swm-simple-ipc-client.h"
#include "ac-config-headers.h"

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#define TERMINATE_READ_DELIMITER 4 /* end of transmission */
#define TERMINATE_CONNECTION_DELIMITER 23 /* end of transmission block */

#define FORMAT_SWM(s) do {                      \
    if (SUPPRESS_OUTPUT == 0) {                 \
      if (PRINT_WITH_SWM_FORMATTING) {          \
        print_with_stumpwm_formatting(s);       \
      } else {                                  \
        printf("%s", s);                        \
      }                                         \
    }                                           \
  } while (0)

static int PRINT_WITH_SWM_FORMATTING = 0;
static int verbose = 0;
static int SUPPRESS_OUTPUT = 0;

#ifdef HAVE_LIBREADLINE
static char **stumpwm_commands = NULL;
#endif

enum request_type {SWM_COMMAND, CHANGE_PACKAGE, IN_PACKAGE, BINDINGS, EVAL,
                   INTERACTIVE, RAW };

struct request {
  enum request_type type;
  char *opt;
};

void check_for_errno(int err);
char *concatenate(int number_of_strings, ...);

void append_command_to_request(struct request *req, char *cmd);
struct request *generate_command_request(char *cmd);
struct request *generate_eval_request(char *ev);
struct request *generate_eval_writeback_request(char *ev);
struct request *generate_package_change_request(char *pkg);
struct request *generate_raw_request(char *str);
struct request *generate_interactive_request();

int send_in_package(int sock, char *pkg);
void send_bindings(int sock, char *bindings);
void send_eval_bindings(int sock, char *bindings);
char *send_request(int sock, struct request *req);
char *send_raw_request(int sock, struct request *req);

char *read_response(int sock);
char *read_from_socket(int socket, char delim);
char *read_from_socket_replace_delim(int socket, char delim, char replacement);

void print_with_stumpwm_formatting(char *s);
int format_verbose(int level, FILE *stream, char *format_control_string, ...);

void parse_stdin_for_commands(char *stdin, struct request **requests);

void display_help(void) {
  printf("\
Usage: swmipc [OPTIONS] ...\n\
Send an IPC request to StumpWM\n\
Example: swmipc --command echo-date\n\
\n\
Execution Options:\n\
  -c, --command COMMAND [...]\n\
      Take in one or more commands and execute them. If COMMAND is a - then\n\
      read from standard input until EOF, treating every line as a command.\n\
  -e, --eval FORM [...]\n\
      Take in one or more lisp forms and evaluate them. If FORM is a - then\n\
      read from standard input until EOF, and wrap everything read in a progn.\n\
  -r, --raw IPC-REQUEST [...]\n\
      Take in one or more raw ipc request messages and send them as is. If \n\
      IPC-REQUEST is a - then read from standard input until EOF.\n\
  -R, --repl TYPE\n\
      Start an interactive session of type TYPE. TYPE may be a one of command,\n\
      eval, or raw. Ordered options provided before and after this option will\n\
      be sent in the order they are recieved, e.g. providing\n\
         \"-c cmd1 -R raw -c cmd2\"\n\
      will first run cmd1, then begin a raw repl and upon its exit, run cmd2.\n\
  -i, --interactive\n\
      Start an interactive session of type command.\n\
\n\
Lisp Environment Options:\n\
  -p, --in-package PACKAGE    Change the package for this invocation of swmipc\n\
  -P, --change-package PACKAGE\n\
                              Change the package for this and future invocations\n\
                              of swmipc.\n\
  -b, --bind SYMBOL VALUE     Bind SYMBOL to VALUE for the duration of the\n\
                              connection. SYMBOL must already exist as a \n\
                              variable and be declared special.\n\
  -B, --bind-eval SYMBOL VALUE\n\
                              The same as --bind, but evaluates VALUE if \n\
                              evaluation is allowed.\n\
\n\
Printing Options:\n\
  -S, --suppress-messages    Bind STUMPWM::*EXECUTING-STUMPWM-COMMAND* to T,\n\
                             suppressing messages in StumpWM.\n\
  -f, --with-stumpwm-formatting\n\
                             Duplicate StumpWM formatting directives when\n\
                             printing.\n\
  -v, --verbose              Inform the user of what is being done internally.\n\
  -q, --quiet                Dont print anything recieved from StumpWM.\n\
\n\
Configuration Options:\n\
  -s, --socket SOCKET    Communicate on socket SOCKET.\n\
  -k, --kill-server      Kill the IPC server after handling all requests.\n\
\n\
Other Options:\n\
  -h, --help    Display this message\n\
  -V, --version Display the version message\n\
\n\
Ordered options are options which are run in the order they are recieved. Other\n\
options are run either before or after all ordered options.\n\
Ordered options are:\n\
  -c, -e, -r, -R, -i, and -P \n\
Unordered options run before ordered options are:\n\
  -s, -C, -b, -B, and -p \n\
Unordered options run after ordered options are:\n\
  -k \n\
All other options have no bearing on order and affect only the client.\n\
");
}

void display_version(void) {
  return;
}

/* return 1 if option is a single dash */
int check_argv_for_dash(char *option) {
  if (option[0] == '-') {
    if (option[1] != '\0') {
      return 0;
    } else {
      return 1;
    }
  } else {
    return 1;
  }
}

int main (int argc, char **argv) {
  struct sockaddr_un addr;
  char *socket_name = SOCKET_PATH;
  static struct option long_options[] =
    {
      /* Execution Options */
      {"command",                 required_argument, NULL, 'c'},
      {"eval",                    required_argument, NULL, 'e'},
      {"raw",                     required_argument, NULL, 'r'},
      {"repl",                    required_argument, NULL, 'R'},
      {"interactive",             no_argument,       NULL, 'i'},
      /* Lisp Environment Options */
      {"in-package",              required_argument, NULL, 'p'},
      {"change-package",          required_argument, NULL, 'P'},
      {"bind",                    required_argument, NULL, 'b'},
      {"bind-eval",               required_argument, NULL, 'B'},
      /* Printing Options */
      {"suppress-messages",       no_argument,       NULL, 'S'},
      {"with-stumpwm-formatting", no_argument,       NULL, 'f'},
      {"verbose",                 no_argument,       NULL, 'v'},
      {"quiet",                   no_argument,       NULL, 'q'},
      /* Configuration Options */
      {"socket",                  required_argument, NULL, 's'},
      /* {"change-server-socket",    required_argument, NULL, 'C'}, */
      {"kill-server",             no_argument,       NULL, 'k'},
      /* {"abort-on-error",          no_argument,       NULL, 'a'}, */
      {"version",                 no_argument,       NULL, 'V'},
      {"help",                    no_argument,       NULL, 'h'},
      {NULL, 0, NULL, 0}
    };

  char /* **requests, */ *bindings, *hold, *in_package, *buf,
    *stdinread, *stdin_ev_hold, *eval_bindings;
  int c, option_index, request_index, sock, checkval;
  int kill_server, i, k;
  struct request **requests;

  option_index = 0;
  requests = malloc(sizeof(struct request *) * (argc + 1));
  memset(requests, 0, (sizeof(struct request *) * (argc + 1)));
  request_index = 0;

  bindings = NULL;
  eval_bindings = NULL;
  in_package = NULL;
  
  kill_server = 0;
    
  while ((c = getopt_long(argc, argv, "c:e:r:R:ip:P:b:B:Sfvqs:khV",
                          long_options, &option_index)) != -1) {
    switch (c)
      {
      case 'c':
        for (k = optind - 1;
             k < argc && check_argv_for_dash(argv[k]);
             k++) {
          if (argv[k][0] == '-') {
            /* read from stdin until EOF, with one command per line */
            stdinread = read_from_socket_replace_delim(STDIN_FILENO, EOF, '\0');
            parse_stdin_for_commands(stdinread, &requests[request_index]);
            free(stdinread);
          } else {
            if (requests[request_index] == NULL) {
              requests[request_index] = generate_command_request(argv[k]);
            } else {
              append_command_to_request(requests[request_index], argv[k]);
            }
          }
        }
        optind = k;
        request_index += 1;
        break;
      case 'e':
        for (k = optind - 1;
             k < argc && check_argv_for_dash(argv[k]);
             k++) {
          if (argv[k][0] == '-') {
            stdinread = read_from_socket_replace_delim(STDIN_FILENO, EOF, '\0');
            stdin_ev_hold = concatenate(3, "(progn ", stdinread, ")");
            free(stdinread);
            requests[request_index] = generate_eval_request(stdin_ev_hold);
            free(stdin_ev_hold);
          } else {
            requests[request_index] = generate_eval_request(argv[k]);
          }
          request_index += 1;
        }
        optind = k;
        break;
      case 'r':
        for (k = optind - 1;
             k < argc && check_argv_for_dash(argv[k]);
             k++) {
          if (argv[k][0] == '-') {
            stdinread = read_from_socket_replace_delim(STDIN_FILENO, EOF, '\0');
            requests[request_index] = generate_raw_request(stdinread);
            free(stdinread);
          } else {
            requests[request_index] = generate_raw_request(argv[k]);
          }
          request_index++;
        }
        break;
      case 'R':
        requests[request_index] = generate_interactive_request(optarg);
        request_index += 1;
        break;
      case 'i':
        requests[request_index] = generate_interactive_request("command");
        request_index += 1;
        break;
      case 'p':
        /* in package wraps the whole call */
        in_package = optarg;
        break;
      case 'P':
        format_verbose(1, stdout, "Got a package: %s\n", optarg);
        if (request_index > 0 &&
            (requests[request_index-1])->type == CHANGE_PACKAGE) {
          (requests[request_index-1])->opt = optarg;
        } else {
          requests[request_index] = generate_package_change_request(optarg);
        }
        break;
      case 'b':
        format_verbose(1, stdout, "Got a binding: %s\n", optarg);
        if (argv[optind] == NULL) {
          fprintf(stderr, "Value is required for bindings\n");
          exit((int)'b');
        } else {
          if (bindings == NULL) {
            bindings = concatenate(5, "(", optarg, " ", argv[optind], ")");
          } else {
            hold = bindings;
            bindings = concatenate(6, hold, " (", optarg, " ", argv[optind], ")");
            free(hold);
          }
          optind += 1;
        }
        break;
      case 'B':
        format_verbose(1, stdout, "Got eval bindings: %s\n", optarg);
        if (argv[optind] == NULL) {
          fprintf(stderr, "Value is required for bindings\n");
          exit((int)'B');
        } else {
          if (bindings == NULL) {
            eval_bindings = concatenate(5, "(", optarg, " ", argv[optind], ")");
          } else {
            hold = eval_bindings;
            eval_bindings =
              concatenate(6, hold, " (", optarg, " ", argv[optind], ")");
            free(hold);
          }
          optind += 1;
        }
        break;        
      case 'S':
        if (bindings == NULL) {
          bindings = concatenate(3, "(",
                                 "STUMPWM::*EXECUTING-STUMPWM-COMMAND* T",
                                 ")");
        } else {
          hold = bindings;
          bindings = concatenate(2, hold,
                                 " (STUMPWM::*EXECUTING-STUMPWM-COMMAND* T)");
          free(hold);
        }
        break;
      case 'f':
        PRINT_WITH_SWM_FORMATTING = 1;
        break;
      case 'v':
        verbose = 1;
        break;
      case 'q':
        SUPPRESS_OUTPUT = 1;
        break;
      case 's':
        format_verbose(1, stdout, "use socket: %s\n", optarg);
        socket_name = optarg;
        break;
      case 'k':
        kill_server = 1;
        break;
      case 'V':
        display_version();
        free(requests);
        exit(0);
        break;
      case 'h':
        display_help();
        free(requests);
        exit(0);        
        break;
      case '?':
        break;
      default:
        printf("Unknown Option\n");
        break;
      }
  }
  requests[request_index] = NULL;

  sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock == -1) {
    fprintf(stderr, "Error opening socket, exiting\n");
    exit(1);
  }

  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  strncpy(addr.sun_path, socket_name, sizeof(addr.sun_path) - 1);

  checkval = connect(sock, (struct sockaddr *) &addr, sizeof(addr));
  if (checkval == -1) {
    fprintf(stderr, "Error connecting socket\n");
    exit(3);
  }

  /* set up the connection */
  if (send_in_package(sock, in_package) == 0) {
    exit(4);
  }
  send_bindings(sock, bindings);
  send_eval_bindings(sock, eval_bindings);
  for (i = 0; i < argc && requests[i] != NULL; i++) {
    buf = send_request(sock, requests[i]);
    if (buf != NULL) {
      FORMAT_SWM(buf);
      free(buf);
    }
  }

  if (kill_server) {
    format_verbose(2, stdout, "killing server\n");
    send_request(sock, generate_raw_request("(:kill-server)"));
  }

  write(sock, "(:end-connection) ", strlen("(:end-connection) "));
  buf = read_from_socket(sock, TERMINATE_CONNECTION_DELIMITER);
  format_verbose(2, stdout, "%s", buf);
  close(sock);
  free(buf);
  free(requests);
  return 0;
}


/***********************/
/** Generate Requests **/
/***********************/

void parse_stdin_for_commands(char *stdin, struct request **requests) {
  int i, j, ci;
  char *walk, *cmd;
  walk = stdin;
  ci = 0;
  while (walk != NULL && *walk != EOF && *walk != '\0') {
    for (i = 0; walk[i] != '\n' && walk[i] != '\0' && walk[i] != EOF; i++);
    cmd = malloc(sizeof(char) * (i + 1));
    for (j = 0; j < i; j++) {
      cmd[j] = walk[j];
    }
    cmd[j] = '\0';

    if (*requests == NULL) {
      *requests = generate_command_request(cmd);
    } else {
      append_command_to_request(*requests, cmd);
    }
    free(cmd);
    if (walk[i] == '\n') {
      walk = &walk[i+1];
    } else {
      walk = &walk[i];
    }
    ci++;
  }
  return;
}

void append_command_to_request(struct request *req, char *cmd) {
  char *hold;
  hold = req->opt;
  req->opt = concatenate(4, hold, " \"", cmd, "\"");
  free(hold);
  return;
}

struct request *generate_command_request(char *cmd) {
  struct request *req;
  req = malloc(sizeof(struct request));
  req->type = SWM_COMMAND;
  req->opt = concatenate(3, "\"", cmd, "\"");
  return req;
}

struct request *generate_eval_request(char *ev) {
  struct request *req;
  req = malloc(sizeof(struct request));
  req->type = EVAL;
  req->opt = strdup(ev);
  return req;
}

struct request *generate_eval_writeback_request(char *ev) {
  struct request *req;
  req = malloc(sizeof(struct request));
  req->type = EVAL;
  req->opt = concatenate(2, ev, " :write-form T");
  return req;
}

struct request *generate_package_change_request(char *pkg) {
  struct request *req;
  req = malloc(sizeof(struct request));
  req->type = CHANGE_PACKAGE;
  req->opt = pkg;
  return req;
}

struct request *generate_raw_request(char *str) {
  struct request *req;
  req = malloc(sizeof(struct request));
  req->type = RAW;
  req->opt = strdup(str);
  return req;
}

struct request *generate_interactive_request(char *type) {
  struct request *req;
  req = malloc(sizeof(struct request));
  req->type = INTERACTIVE;
  req->opt = type;
  return req;
}


/*******************/
/** Send Requests **/
/*******************/

void send_to_socket(int sock, char *string) {
  int len, ret;
  len = strlen(string);
  ret = write(sock, string, len);
  if (ret == -1) {
    fprintf(stderr, "Error writing to socket\n");
    return;
  } else if (ret < len) {
    send_to_socket(sock, &string[ret]);
    return;
  } else {
    write(sock, " ", 1);
    if (ret == -1 || ret == 0) {
      fprintf(stderr, "Error writing to socket\n");
    }
    return;
  }
  return;
}

void send_eval_bindings(int sock, char *bindings) {
  char *transmit;
  if (bindings != NULL) {
    transmit  = concatenate(3, "(:bind-special-variables (", bindings,
                            ") :eval-values t)");
    format_verbose(1, stdout, "bindings: %s\n", transmit);
    free(bindings);
    send_to_socket(sock, transmit);
    free(transmit);
    transmit = read_response(sock);
    if (transmit[0] != 4 && transmit[0] != 0) {
      FORMAT_SWM(transmit);
    }
    free(transmit);
  }
  return;
}

void send_bindings(int sock, char *bindings) {
  char *transmit;
  if (bindings != NULL) {
    transmit  = concatenate(3, "(:bind-special-variables (", bindings, "))");
    format_verbose(1, stdout, "bindings: %s\n", transmit);
    free(bindings);
    send_to_socket(sock, transmit);
    free(transmit);
    transmit = read_response(sock);
    if (transmit[0] != 4 && transmit[0] != 0) {
      FORMAT_SWM(transmit);
    }
    free(transmit);
  }
  return;
}

int send_in_package(int sock, char *pkg) {
  char *transmit;
  int ret = 1;
  format_verbose(1, stdout, "In package: %s\n", pkg);
  if (pkg != NULL) {
    transmit = concatenate(3, "(:change-package ", strdup(pkg), " :reset t)");
    send_to_socket(sock, transmit);
    free(transmit);
    transmit = read_response(sock);
    if (strncmp(transmit, "^[^1[ERROR]^]", 13) == 0) {
      FORMAT_SWM(transmit);
      ret = 0;
    }
    free(transmit);
  }
  return ret;
}

char *send_raw_request(int sock, struct request *req) {
  format_verbose(2, stdout, "handling: %s\n", req->opt);
  send_to_socket(sock, req->opt);
  free(req->opt);
  return read_response(sock);
}

char *send_command_request(int sock, struct request *req) {
  char *transmit = concatenate(3, "(:swm-command ", req->opt, " :write-back-immediately t)");
  format_verbose(1, stdout, "sending: %s\n", transmit);
  free(req->opt);
  send_to_socket(sock, transmit);
  free(transmit);
  return read_response(sock);
}

char *send_package_change_request(int sock, struct request *req) {
  char *transmit = concatenate(3, "(:change-package ", req->opt, ")");
  send_to_socket(sock, transmit);
  free(transmit);
  return read_response(sock);
}

char *send_eval_request(int sock, struct request *req) {
  char *transmit = concatenate(3, "(:evaluate-form ", req->opt, " :writeback t)");
  free(req->opt);
  send_to_socket(sock, transmit);
  free(transmit);
  transmit = read_response(sock);
  return transmit;
}

#ifdef HAVE_LIBREADLINE
void request_command_list(int sock) {
  char *req, *walk, *cmd;
  int l, i, c, j, ci;
  send_to_socket(sock, "(:information-request :swm-command-list)");
  req = read_response(sock);
  l = strlen (req);
  /* get the number of commands present */
  for (i = 0, c = 0; i < l; i++) {
    if (req[i] == 3) {
      c++;
    }
  }
  if (stumpwm_commands != NULL) {
    for (i = 0; stumpwm_commands[i] != NULL; i++) {
      free(stumpwm_commands[i]);
    }
    free(stumpwm_commands);
  }
  stumpwm_commands = malloc(sizeof(char*) * (c + 2));
  stumpwm_commands[c] = NULL;
  walk = req;
  ci = 0;
  while (walk != NULL && (*walk) != 4 && (*walk) != '\0') {
    for (i = 0; walk[i] != 3 && walk[i] != 4 && walk[i] != '\0'; i++);
    cmd = malloc(sizeof(char) * (i + 1));
    for (j = 0; j < i; j++) {
      cmd[j] = walk[j];
    }
    cmd[j] = '\0';
    stumpwm_commands[ci] = cmd;
    if (walk[i] == 3) {
      walk = &(walk[i+1]);
    } else {
      walk = &(walk[i]);
    }
    ci++;
  }
  stumpwm_commands[ci] = NULL;
  free(req);
}

char *complete_swm_command(const char *text, int state) {
  static int command_index = 0;
  static int length;
  if (stumpwm_commands == NULL) {
    return NULL;
  }
  if (state == 0) {
    command_index = 0;
    length = strlen(text);
  }
  while (stumpwm_commands[command_index] != NULL) {
    command_index++;
    if (strncmp(stumpwm_commands[command_index - 1], text, length) == 0) {
      return strdup(stumpwm_commands[command_index - 1]);
    }
  }
  return NULL;
}

char **command_completer(const char *text, int start,
                         int end __attribute__((unused))) {
  char **matches;
  matches = NULL;
  if (start == 0) {
    matches = rl_completion_matches(text, complete_swm_command);
  }
  return matches;
}

void initialize_readline_for_swm(int sock) {
  rl_readline_name = "swmipc";
  rl_attempted_completion_function = command_completer;
  request_command_list(sock);
  return;
}

int read_one_line (char **linevar, char *prompt,
                   size_t *len  __attribute__ ((unused))) {
  *linevar = readline(prompt);
  if (*linevar == NULL) {
    return -1;
  } else {
    add_history(*linevar);
    return strlen(*linevar);
  }
}

#else

int read_one_line (char **linevar, char *prompt, size_t *len) {
  int lenr;
  printf("%s", prompt);
  lenr = getline(linevar, len, stdin);
  if (lenr != -1) {
    if ((*linevar)[0] == '\n') {
      free(*linevar);
      *linevar = NULL;
      return 0;
    }
    (*linevar)[lenr-1] = '\0';
  }
  return lenr;
}
#endif

char *begin_interactive_session(int sock, char *type) {
  char *line, *buf;
  size_t len;
  int lenr, cmd_eval_raw;
  format_verbose(1, stderr, "beginning interactive session\n");
  
  if (strcmp(type, "command") == 0) {
    cmd_eval_raw = 1;
  } else if (strcmp(type, "eval") == 0)  {
    cmd_eval_raw = 2;
  } else if (strcmp(type, "raw") == 0)  {
    cmd_eval_raw = 3;
  } else {
    fprintf(stderr, "%s is not a valid type of interactive session\n", type);
    return NULL;
  }
  
  
#ifdef HAVE_LIBREADLINE
  initialize_readline_for_swm(sock);
#endif
  
  while (1) {
    len = 0;
    line = NULL;
    lenr = read_one_line(&line, "swmipc> ", &len);
    if (lenr == -1) {
      free(line);
#ifndef HAVE_LIBREADLINE
      printf("\n");
#endif
      return NULL;
    } else if (line != NULL) {
      if (strncmp(line, "swmipc quit", 11) == 0) {
        free(line);
        break;
      } else if (strncmp(line, "swmipc eval", 11) == 0) {
        buf = concatenate(3, "(:evaluate-form ", &line[11], " :writeback t)");
        free(line);
        send_to_socket(sock, buf);
        free(buf);
        buf = read_response(sock);
      } else if (strncmp(line, "swmipc raw", 10) == 0) {
        buf = &line[10];
        send_to_socket(sock, buf);
        free(line);
        buf = read_response(sock);
      } else if (strncmp(line, "swmipc command", 14) == 0) {
        buf = send_command_request(sock, generate_command_request(&line[14]));
        free(line);
      } else {
        switch (cmd_eval_raw)
          {
          case 1:
            buf = send_request(sock, generate_command_request(line));
            break;
          case 2:
            buf = send_eval_request(sock, generate_eval_request(line));
            break;
          case 3:
            send_to_socket(sock, line);
            buf = read_response(sock);
            break;
          }
        free(line);
      }
      FORMAT_SWM(buf);
      free(buf);
    }
  }
  return NULL;
}

char *send_request(int sock, struct request *req) {
  char *buf;
  switch (req->type)
    {
    case SWM_COMMAND:
      buf = send_command_request(sock, req);
      break;
    case EVAL:
      buf = send_eval_request(sock, req);
      break;
    case INTERACTIVE:
      buf = begin_interactive_session(sock, req->opt);
      break;
    case RAW:
      buf = send_raw_request(sock, req);
      break;
    default:
      buf = strdup("UNSUPPORTED REQUEST TYPE\n");
      break;
    }
  free(req);
  return buf;
}


/*******************/
/** Read Response **/
/*******************/

char *read_response(int sock) {
  return read_from_socket(sock, TERMINATE_READ_DELIMITER); 
}

/*
  read one character at a time until delim is encountered.
*/

char *read_from_socket_replace_delim(int socket, char delim, char replacement) {
  char *ret, *hold;
  size_t i, size;
  int multiple, eofp;
  multiple = 1;
  size = 1024;
  eofp = 0;

  ret = malloc(sizeof(char) * size);
  ret[0] = '\0';
  ret[1023] = '\0';
  i = 0;

  while ((eofp = read(socket, &(ret[i]), 1)) == 1) {
    if (i == ((size * multiple) - 1)) {
      hold = ret;
      ret = malloc(sizeof(char) * (size * (multiple + 1)));
      memcpy (ret, hold, (size * multiple));
      free(hold);
      multiple++;
    }
    ret[i+1] = '\0';
    if (ret[i] == delim) {
      ret[i] = replacement;
      break;
    }
    i++;
  }
  return ret;
}

char *read_from_socket(int socket, char delim) {
  return read_from_socket_replace_delim(socket, delim, delim);
}


/***************/
/** Utilities **/
/***************/

char *concatenate(int number_of_strings, ...) {
  va_list valist;
  int i, j, total_len, holder_len;
  char **holder, *result, *walk;

  holder = malloc(sizeof(char*) * number_of_strings);
  va_start(valist, number_of_strings);
  for (i = 0, total_len = 0; i < number_of_strings; i++) {
    holder[i] = va_arg(valist, char*);
    total_len += strlen(holder[i]);
  }
  result = malloc(sizeof(char) * (total_len + 1));
  result[total_len] = '\0';
  walk = result;
  for (i = 0; i < number_of_strings; i++) {
    holder_len = strlen(holder[i]);
    for (j = 0; j < holder_len; j++) {
      walk[j] = (holder[i])[j];
    }
    walk = &(walk[j]);
  }
  free(holder);
  va_end(valist);
  return result;
}

int number_char_p(char c) {
  switch (c){
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    return 1;
    break;
  }
  return 0;
}

void print_with_stumpwm_formatting(char *s) {
  /*
    attempt to respect stumpwm formatting directives where we can. Currently,
    colors, reverse, bright, normal, and literal ^ are supported. 
  */
  int breaker = 1;
  int i = 0;
  char fg, bg;
  char color_stack[20] = {'\0', '\0', '\0', '\0', '\0',
                          '\0', '\0', '\0', '\0', '\0',
                          '\0', '\0', '\0', '\0', '\0',
                          '\0', '\0', '\0', '\0', '\0'};
  /* color stack is stored two at a time, eg when pushing colors we set the
     current index with the foreground and the next index with the background
     and increment the index by 2. or something like that, idk. */
  int cs_index = 0;
  fg = 127;
  bg = 127;
  while (breaker) {
    if (s[i] == '\0') {
      breaker = 0;
    } else if (s[i] == '^')  {
      i++;
      if (s[i] == '\0') {
        breaker = 0;
      } else if (number_char_p(s[i])) {
        printf("\x1b[3%cm", s[i]);
        fg = s[i];
        i++;
        if (number_char_p(s[i])) {
          printf("\x1b[4%cm", s[i]);
          bg = s[i];
          i++;
        } else if (s[i] == '*') {
          i++;
        }
      } else {
        switch (s[i])
          {
          case 'n':
            printf("\x1b[0m");
            break;
          case 'R':
            printf("\x1b[7m");
            break;
          case 'r':
            printf("\x1b[27m");
            break;
          case 'B':
            printf("\x1b[1m");
            break;
          case 'b':
            printf("\x1b[22m");
            break;
          case '^':
            printf("^");
            break;
          case '[':
            if (cs_index < 19) {
              color_stack[cs_index] = fg;
              color_stack[cs_index + 1] = bg;
              cs_index += 2;
            }
            break;
          case ']':
            if (cs_index > 0) {
              cs_index -= 2;
              fg = color_stack[cs_index];
              bg = color_stack[cs_index + 1];
              color_stack[cs_index] = '\0';
              color_stack[cs_index + 1] = '\0';
              if (fg == 127 && bg == 127) {
                printf("\x1b[0m");
              } else {
                printf("\x1b[3%cm", fg);
                printf("\x1b[4%cm", bg);
              }
            }
            break;
          default:
            break;
          }
        i++;
      }
    } else if (s[i] != TERMINATE_READ_DELIMITER) {
      printf("%c", s[i]);
      i++;
    } else {
      i++;
    }
  }
  printf("\x1b[0m");
  return;
}

int format_verbose(int level, FILE *stream, char *format_control_string, ...) {
  int ret = 0;
  va_list vargs;
  if (verbose >= level) {
    va_start(vargs, format_control_string);
    ret = vfprintf(stream, format_control_string, vargs);
    va_end(vargs);
  }
  return ret;
}


  
