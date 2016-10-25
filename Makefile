CXXFLAGS =	-O2 -g -Wall -fmessage-length=0 -std=gnu++11 $(INCLUDES)

OBJS = main.o

INCLUDES = -IPEGTL -I.

TARGET = main

main.o: expr.hh
expr.o: expr.hh

$(TARGET): $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS)

all:	$(TARGET)

clean:
	rm -f $(OBJS) $(TARGET)

test/test.o: expr.hh test/test.cc
TEST_OBJS = test/test.o test/catch.o

test/test: $(TEST_OBJS)
	$(CXX) -o test/test $(TEST_OBJS)

test: test/test
	test/test $(word 2, $(MAKECMDGOALS))

%:
	@: