CXXFLAGS =	-O2 -g -Wall -fmessage-length=0 -std=gnu++11 $(INCLUDES)
CXX = clang++

OBJS = main.o

INCLUDES = -IPEGTL -I.

TARGET = main

main.o: expr.hh
expr.o: expr.hh

$(TARGET): $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS)

all:	$(TARGET)

clean: test_clean
	rm -f $(OBJS) $(TARGET)

test_clean:
	rm -f $(TEST_OBJS) $(TEST_TARGET)

TEST_TARGET = test/test

test/test.o: expr.hh test/test.cc
TEST_OBJS = test/test.o test/catch.o

$(TEST_TARGET): $(TEST_OBJS)
	$(CXX) -o $(TEST_TARGET) $(TEST_OBJS)

test: $(TEST_TARGET)
	$(TEST_TARGET) $(word 2, $(MAKECMDGOALS))

%:
	@:
