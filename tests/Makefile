VPATH = ../common
INCLUDES = -I../common
CFLAGS += -Wall $(INCLUDES) `pkg-config --cflags glib-2.0 gobject-2.0`
LDLIBS += `pkg-config --libs glib-2.0 gobject-2.0`

SOURCES = test_preferences.c ../common/preferences.c
OBJECTS = $(SOURCES:.c=.o)
TARGET = test_preferences
CLEANABLES = $(OBJECTS) $(TARGET)

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@ $(LDLIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(CLEANABLES)

.PHONY: all clean
