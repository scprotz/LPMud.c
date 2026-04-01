CFLAGS = -O -DMUD_LIB='"$(MUD_LIB)"'

make_docs:

clean:
	-rm *.o make_docs 2>/dev/null || true

install: make_docs
	install -c -s $? $(BINDIR)
