CXXFLAGS =	-O2 -g -Wall -fmessage-length=0

OBJS =		libexpr2.o

LIBS =

TARGET =	libexpr2

$(TARGET):	$(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

all:	$(TARGET)

clean:
	rm -f $(OBJS) $(TARGET)
