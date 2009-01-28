CC= ghc
OBJS= docsetview.o
OUTPUT= docsetview
LDFLAGS=  -o docsetview -package HDBC -package HDBC-sqlite3 -package regex-base -package regex-posix

all: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS)

%.o : %.hs
	$(CC) $(CFLAGS) -c $*.hs
