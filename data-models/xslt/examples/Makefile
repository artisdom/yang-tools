HELLO = hello-server.xml
HERE = $(shell pwd)
TARGET ?= get-config-reply
BASE ?= test
SCHEMAS = $(BASE)-$(TARGET).rng $(BASE)-$(TARGET).sch $(BASE)-$(TARGET).dsrl
INSTANCE = $(BASE)-$(TARGET).xml
Y2DOPTS = -t $(TARGET) -b $(BASE)
DSRL2XSL = ../../../pyang/xslt/dsrl2xslt.xsl
MODULEDIR = ../..
SSHEET = ../cisco-ios/cisco-ios.xsl
DFLTSHEET = $(BASE)-$(TARGET)-defaults.xsl
IOS = test-cisco-ios.cfg
.PHONY: all clean rnc validate

all: $(SCHEMAS) $(IOS)

rnc: $(BASE)-$(TARGET).rnc

$(IOS): $(DFLTSHEET) $(INSTANCE) ietf-system.yin
	xsltproc $< $(word 2,$^) | \
	    xsltproc --output $@ --stringparam system-yin \
	    $(HERE)/$(word 3,$^) $(SSHEET) -

clean:
	rm -f $(SCHEMAS) $(IOS) $(DFLTSHEET) ietf-system.yin \
	    model.* *-gdefs*.rng *.rnc

validate: $(INSTANCE) $(SCHEMAS)
	yang2dsdl -j -s $(Y2DOPTS) -v $<

$(SCHEMAS): $(HELLO)
	yang2dsdl -L $(Y2DOPTS) $(HELLO)

%.rnc: %.rng
	trang -I rng -O rnc $< $@
 
%-defaults.xsl: %.dsrl
	xsltproc --output $@ $(DSRL2XSL) $<

ietf-system.yin: $(MODULEDIR)/system/ietf-system.yang
	pyang -f yin -o $@ $<

model.dsdl: $(YMODULES)
	pyang -L -f dsdl --dsdl-no-documentation $(HELLO) \
	    | xmllint --format - > $@

model.tree: $(YMODULES)
	pyang -L -f tree -o $@ $(HELLO)

