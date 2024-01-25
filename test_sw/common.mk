basedir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
srcdir := $(basedir)/src

PROGRAMS ?= test

CC := $(TARGET)-gcc
OBJDUMP := $(TARGET)-objdump

CFLAGS += -I $(srcdir)
CFLAGS += -I /usr/include/
CFLAGS += -I /usr/lib/gcc/riscv64-unknown-elf/9.3.0/include/

hdrs := $(wildcard *.h) $(wildcard $(srcdir)/*.h)
objs ?=
ldscript ?=

%.o: $(srcdir)/%.c $(hdrs)
	$(CC) $(CFLAGS) -o $@ -c $<
%.o: $(srcdir)/%.c $(hdrs)
	$(CC) $(CFLAGS) -o $@ -c $<
%.o: $(srcdir)/%.S $(hdrs)
	$(CC) $(CFLAGS) -D__ASSEMBLY__=1 -o $@ -c $<

%.riscv: %.o $(objs) $(ldscript)
	$(CC) -O0 $(LDFLAGS) $(if $(ldscript),-T $(ldscript)) -o $@ $< $(objs)

%.dump: %.riscv
	$(OBJDUMP) -d $< > $@

.DEFAULT: elf

.PHONY: elf dumps
elf: $(addsuffix .riscv,$(PROGRAMS))
dumps: $(addsuffix .dump,$(PROGRAMS))

.PHONY: clean
clean:
	rm -f -- *.riscv *.o *.dump

.SUFFIXES:
.SUFFIXES: .o .c .S
