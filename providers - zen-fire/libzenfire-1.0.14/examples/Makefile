
################################################################################
################################################################################

BIN=account_monitor market_data market_data_detailed place_order
OBJS=$(am_OBJ) $(md_OBJ) $(mdd_OBJ) $(po_OBJ)

am_OBJ=account_monitor.o
md_OBJ=market_data.o
mdd_OBJ=market_data_detailed.o
po_OBJ=place_order.o

target_os=$(shell uname -s)

$(info configuring for $(target_os))

ifeq ($(target_os),Linux)
  LIBZENFIRE=../lib/linux/lib32/libzenfire.a
  LIBS=$(LIBZENFIRE) -lpthread -lrt -ldl
  CPPFLAGS+=-m32 -I../include
  LDFLAGS+=-m32

else ifeq ($(target_os),Darwin)
  LIBZENFIRE=../darwin/lib/libzenfire.a
  LIBS=$(LIBZENFIRE) -lssl -lcrypto -lz
  CPPFLAGS+=-I../include
  LDFLAGS+=

else
  $(error unknown target os '$(target_os)')
endif

.PHONY: all clean test
all : $(BIN)

clean:
	-$(RM) $(OBJS) $(BIN)

test:
	@echo no tests

################################################################################

account_monitor: $(am_OBJ) $(LIBZENFIRE)
	$(CXX) $(LDFLAGS) $(am_OBJ) -o "$@" $(LIBS)

market_data: $(md_OBJ) $(LIBZENFIRE)
	$(CXX) $(LDFLAGS) $(md_OBJ) -o "$@" $(LIBS)

market_data_detailed: $(mdd_OBJ) $(LIBZENFIRE)
	$(CXX) $(LDFLAGS) $(mdd_OBJ) -o "$@" $(LIBS)

place_order: $(po_OBJ) $(LIBZENFIRE)
	$(CXX) $(LDFLAGS) $(po_OBJ) -o "$@" $(LIBS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $< -o $@

################################################################################
################################################################################

