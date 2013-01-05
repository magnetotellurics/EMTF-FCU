#!/usr/bin/make

# FOX_LIB has to be the path (or relative path) to the FoX library on your system
FOX_LIB = ../FoX

include $(FOX_LIB)/arch.make

PROGS = z2edi xml2z z2xml xml2edi z2z
BIN_DIR = /home/data/bin.ubuntu #~/data/bin
OBJS_Z2XML   = global.o parse_dom.o z_read.o xml_write.o read_lists.o utils.o config.o
OBJS_XML2Z   = global.o parse_dom.o xml_read.o z_write.o
OBJS_XML2EDI = global.o parse_dom.o xml_read.o edi_write.o utils.o
OBJS_Z2EDI   = global.o z_read.o edi_write.o utils.o
OBJS_Z2Z     = global.o z_read.o z_write.o utils.o
CONFIG=$(FOX_LIB)/FoX-config
INCFLAGS=$$($(CONFIG) --fcflags --wxml)
LIBS=$(FOX_LIB)/objs/lib/libFoX_dom.a $(FOX_LIB)/objs/lib/libFoX_wxml.a $(FOX_LIB)/objs/lib/libFoX_common.a

all: $(PROGS) install

z2xml: z2xml.o $(LIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< $(OBJS_Z2XML) $$($(CONFIG) --libs --dom)

xml2z: xml2z.o $(LIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< $(OBJS_XML2Z) $$($(CONFIG) --libs --dom)

xml2edi: xml2edi.o $(LIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< $(OBJS_XML2EDI) $$($(CONFIG) --libs --dom)

z2edi: z2edi.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< $(OBJS_Z2EDI) $$($(CONFIG) --libs --dom)

z2z: z2z.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< $(OBJS_Z2Z) $$($(CONFIG) --libs --dom)

z2xml.o: z_read.o xml_write.o read_lists.o config.o

xml2z.o: xml_read.o z_write.o

xml2edi.o: xml_read.o edi_write.o

z2edi.o: z_read.o edi_write.o

z2z.o: z_read.o z_write.o

z_read.o: global.o utils.o

xml_read.o: global.o parse_dom.o

z_write.o: global.o

xml_write.o: global.o

edi_write.o: global.o utils.o

read_lists.o: global.o parse_dom.o

config.o: global.o 
  
clean:
	rm -f *.o *.mod *.*dump

cleanall: clean
	rm *.exe

install: 
	cp $(PROGS) *.pl $(BIN_DIR)
