CC=clang
LLC=llc
LLCFLAGS=--march=riscv32
CFLAGS=-S -emit-llvm -fno-discard-value-names --target=riscv32-unknown-elf -O2
HOSTCFLAGS=-S -emit-llvm -fno-discard-value-names -O2 -m32

all: builtins.s builtins.x86.ll

builtins.s: builtins.ll
	$(LLC) $(LLCFLAGS) $<

%.ll: %.ll.in
	sed 's/string_/string./g;s/array_/array./g;s/string.add/"string.+"/g;s/string.compare/"string.<=>"/g' $< > $@

builtins.ll.in: builtins.c
	$(CC) $(CFLAGS) $< -o $@

builtins.x86.ll.in: builtins.c
	$(CC) $(HOSTCFLAGS) $< -o $@

clean:
	rm -f builtins{,.x86}.{s,ll,ll.in}
