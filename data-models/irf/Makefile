MODULES = irf-topology
YMOD = $(addsuffix .yang, $(MODULES))
TARGET ?= config
BASE ?= irf
Y2Y = ~/Projects/yang-tools/xslt/yin2yang.xsl
SCHEMAS = $(BASE)-$(TARGET).rng $(BASE)-$(TARGET).sch $(BASE)-$(TARGET).dsrl
XINSTANCE = $(BASE)-$(TARGET).xml
JINSTANCE = $(BASE)-$(TARGET).json
Y2DOPTS = -t $(TARGET) -b $(BASE)
.PHONY = all json clean validate

all: $(SCHEMAS) $(BASE)-$(TARGET).rnc	

json: $(JINSTANCE)

model.xsl: $(YMOD)
	pyang -o $@ -f jsonxsl $^

model.jtox: $(YMOD)
	pyang -o $@ -f jtox $^

model.tree: $(YMOD)
	pyang -o $@ -f tree $^

model.dsdl: $(YMOD)
	pyang -f dsdl --dsdl-no-documentation --dsdl-no-dublin-core $^ | \
	    xmllint --output $@ --format -

$(JINSTANCE): model.xsl $(XINSTANCE)
	xsltproc -o $@ $^

$(SCHEMAS): $(YMOD)
	yang2dsdl $(Y2DOPTS) $^

%.yang: %.yinx
	xsltproc --output $@ $(Y2Y) $<

%.rnc: %.rng
	trang -I rng -O rnc $< $@

clean:
	rm -f $(SCHEMAS) $(JINSTANCE) $(YMOD) model.* *-gdefs.rng *.rnc

validate: $(XINSTANCE) $(SCHEMAS)
	yang2dsdl -j -s $(Y2DOPTS) -v $<


