.PHONY: clean

lemonade-c: lemonade.c
	gcc -o $@ $^ -lClp

lemonade-hs: lemonade.hs
	stack build :lemonade

clean:
	rm -f lemonade-c lemonade.o
