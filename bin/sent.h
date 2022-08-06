struct sentence {
    char *verb;
    struct object *ob;
    char *function;
    struct sentence *next;
    unsigned short short_verb;	/* Only leading characters count */
    unsigned char no_space;
};

struct sentence *alloc_sentence();

void remove_sent(struct object *, struct object *),
    free_sentence(struct sentence *);
