check_boxsys.bin: check_boxsys.c
	gcc -o check_boxsys.bin -O3 -lpthread check_boxsys.c
