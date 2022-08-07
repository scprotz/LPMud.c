#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
/*
 * Some structure forward declarations are needed.
 */
struct program;
struct function;
struct svalue;
struct sockaddr;

//#ifdef BUFSIZ
//#    definexx
//#else /* BUFSIZ */
//#    definex) (
//#endif /* BUFSIZ */

//#ifdef __STDC__
//#    definex x
//#else /* __STDC__ */
//#    definex) (
//#endif /* __STDC */

#ifndef MSDOS
#if defined(sun) && defined(__STDC__)
#ifdef BUFSIZ
int fprintf(FILE *, char *, ...);
int fputs(char *, FILE *);
int fputc(char, FILE *);
int fwrite(char *, int, int, FILE *);
int fread(char *, int, int, FILE *);
#endif
int printf(char *, ...);
int sscanf(char *, char *, ...);
void perror(char *);
#endif

//int read(int, char *, int);
//#ifndef _AIX
//char *malloc(unsigned);
//char *realloc(char *, unsigned);
//#endif
//#ifndef sgi
//int mkdir(char *, int);
//#endif
int fclose(FILE*);
int pclose(FILE*);
//int atoi(char *);
//#ifndef sgi
//void srandom(int);
//#endif
//int chdir(char *);
//int gethostname(char *, int);
void abort(void);
int fflush(FILE*);
//int rmdir(char *);
//int unlink(char *);
int fclose(FILE*);
//#ifndef M_UNIX
//#ifndef sgi
//int system(char *);
//#endif
//#endif
//void qsort(char *, int, int, int (*)());
int fseek(FILE*, long, int);
int _flsbuf();
int fork(void);
int wait(int*);
//int execl(char *, char *, ...);
int pipe(int*);
int dup2(int, int);
int vfork(void);
//void free(char *);
void exit(int);
//int _exit(int);
unsigned int alarm(unsigned int);
//int ioctl(int, ...);
int close(int);
//int write(int, char *, int);
int _filbuf();
//char *crypt(char *, char *);
//#ifdef sun
//char *_crypt(char *, char *);
//#endif

#ifdef DRAND48
double drand48(void);
void srand48(long);
#endif
#ifdef RANDOM
long random(void);
#endif

//long strtol(char *, char **, int);
//int link(char *, char *);
//int unlink(char *);
#endif

struct object;
char* get_error_file(char*);
void save_error(const char*, char*, int);
int write_file(char*, char*);
int file_size(char*);
char* check_file_name(char*, int);
void remove_all_players(void);
void load_wiz_file(void);
void wizlist(char*);
void backend(void);
char* xalloc(int);
void init_string_space(void);
void error();
void fatal();
void add_message();
void trace_log();
void debug_message();
void debug_message_value(struct svalue*), print_local_commands(), new_call_out(
		struct object*, char*, int, struct svalue*), add_action(char*, char*,
		int), list_files(char*), enable_commands(int), load_ob_from_swap(
		struct object*);
int tail(char*);
struct object* get_interactive_object(int);
void enter_object_hash(struct object*);
void remove_object_hash(struct object*);
struct object* lookup_object_hash(char*);
int show_otable_status(int verbose);
void dumpstat(void);
struct vector;
void free_vector(struct vector*);
char* query_load_av(void);
void update_compile_av(int);
struct vector* map_array(struct vector *arr, char *func, struct object *ob,
		struct svalue *extra);
struct vector* make_unique(struct vector *arr, char *func,
		struct svalue *skipnum);

char* describe_items(struct svalue *arr, char *func, int live);
struct vector* filter(struct vector *arr, char *func, struct object *ob,
		struct svalue*);
int match_string(char*, char*);
int set_heart_beat(struct object*, int);
struct object* get_empty_object(int);
struct svalue;
void assign_svalue(struct svalue*, struct svalue*);
void assign_svalue_no_free(struct svalue *to, struct svalue *from);
void free_svalue(struct svalue*);
char* make_shared_string(char*);
void free_string(char*);
int add_string_status(int verbose);
void notify_no_command(void);
void clear_notify(void);
void throw_error(void);
void set_living_name(struct object*, char*);
void remove_living_name(struct object*);
struct object* find_living_object(char*, int);
int lookup_predef(char*);
//void yyerror(char *);
int hashstr(char*, int, int);
int lookup_predef(char*);
char* dump_trace(int);
int parse_command(char*, struct object*);
struct svalue* apply(char*, struct object*, int);
void push_string(char*, int);
void push_number(int);
void push_object(struct object*);
struct object* clone_object(char*);
void init_num_args(void);
int restore_object(struct object*, char*);
void tell_object(struct object*, char*);
struct object* first_inventory(struct svalue*);
struct vector* slice_array(struct vector*, int, int);
int query_idle(struct object*);
char* implode_string(struct vector*, char*);
struct object* query_snoop(struct object*);
struct vector* all_inventory(struct object*);
struct vector* deep_inventory(struct object*, int);
struct object* environment(struct svalue*);
struct vector* add_array(struct vector*, struct vector*);
char* get_f_name(int);
//#ifndef _AIX
//void startshutdowngame(void);
//#else
void startshutdowngame(int);
//#endif
void set_notify_fail_message(char*);
int swap(struct object*);
int transfer_object(struct object*, struct object*);
struct vector* users(void);
void do_write(struct svalue*);
void log_file(char*, char*);
int remove_call_out(struct object*, char*);
char* create_wizard(char*, char*);
void destruct_object(struct svalue*);
void set_snoop(struct object*, struct object*);
int new_set_snoop(struct object*, struct object*);
void add_verb(char*, int);
void ed_start(char*, char*, struct object*);
void say(struct svalue*, struct vector*);
void tell_room(struct object*, struct svalue*, struct vector*);
void shout_string(char*);
int command_for_object(char*, struct object*);
int remove_file(char*);
int print_file(char*, int, int);
int print_call_out_usage(int verbose);
int input_to(char*, int);
int parse(char*, struct svalue*, char*, struct svalue*, int);
struct object* object_present(struct svalue*, struct object*);
void add_light(struct object*, int);
int indent_program(char*);
void call_function(struct program*, struct function*);
void store_line_number_info(void);
void push_constant_string(char*);
void push_svalue(struct svalue*);
struct variable* find_status(char*, int);
void free_prog(struct program*, int);
void stat_living_objects(void);
int heart_beat_status(int verbose);
void opcdump(void);
void slow_shut_down(int);
struct vector* allocate_array(int);
void yyerror(const char*);
void reset_machine(int);
void clear_state(void);
void load_first_objects(void);
void preload_objects(int);
int random_number(int);
void reset_object(struct object*, int);
int replace_interactive(struct object *ob, struct object *obf, char*);
char* get_wiz_name(char*);
char* get_log_file(char*);
int get_current_time(void);
char* time_string(int);
char* process_string(char*);
void update_ref_counts_for_players(void);
void count_ref_from_call_outs(void);
void check_a_lot_ref_counts(struct program*);
int shadow_catch_message(struct object *ob, char *str);
struct vector* get_all_call_outs(void);
char* read_file(char *file, int, int);
char* read_bytes(char *file, int, int);
int write_bytes(char *file, int, char *str);
struct wiz_list* add_name(char *str);
char* check_valid_path(char*, struct wiz_list*, char*, int);
int get_line_number_if_any(void);
void logon(struct object *ob);
struct svalue* apply_master_ob(char *fun, int num_arg);
void assert_master_ob_loaded();
struct vector* explode_string(char *str, char *del);
char* string_copy(const char*);
int find_call_out(struct object *ob, char *fun);
void remove_object_from_stack(struct object *ob);
//#ifndef sgi
//int getpeername(int, struct sockaddr *, int *);
//void  shutdown(int, int);
//#endif
void compile_file(void);
void unlink_swap_file();
char* function_exists(char*, struct object*);
void set_inc_list(struct svalue *sv);
int legal_path(char *path);
struct vector* get_dir(char *path);
//#if !defined(ultrix) && !defined(M_UNIX) && !defined(sgi)
//extern int rename(char *, char *);
//#endif
void get_simul_efun(struct svalue*);
struct function* find_simul_efun(char*);
char* query_simul_efun_file_name(void);
struct vector* match_regexp(struct vector *v, char *pattern);

#ifdef MUDWHO
void sendmudwhoinfo(void);
void sendmudwhologout(struct object *ob);
int rwhocli_setup PROT((char *server, char *serverpw, char *myname,
			char *comment));
int rwhocli_shutdown(void);
int rwhocli_pingalive(void);
int rwhocli_userlogin(char *uid, char *name, int tim);
int rwhocli_userlogout(char *uid);
#endif /* MUDWHO */
