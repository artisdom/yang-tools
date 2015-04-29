SUBMOD = bird-global bird-device bird-kernel bird-nest bird-ospf
EXAMPLE_BASE = sample
EXAMPLE_TYPE = config
EXAMPLE_INST = $(baty).xml

baty = $(EXAMPLE_BASE)-$(EXAMPLE_TYPE)
yams = bird.yang $(addsuffix .yang, $(SUBMOD)) 
xsldir = ../../xslt
schemas = $(baty).rng $(baty).sch $(baty).dsrl
y2dopts = -t $(EXAMPLE_TYPE) -b $(EXAMPLE_BASE)

.PHONY: all clean

all: $(yams)
	pyang bird.yang

%.yang: %.yinx
	xsltproc --output $@ $(xsldir)/yin2yang.xsl $<

$(schemas): hello.xml $(yams)
	yang2dsdl $(y2dopts) -L $<

%.rnc: %.rng
	trang -I rng -O rnc $< $@

rnc: $(baty).rnc

validate: $(EXAMPLE_INST) $(schemas)
	@yang2dsdl -j -s $(y2dopts) -v $<

model.tree: hello.xml
	pyang $(PYANG_OPTS) -f tree -o $@ -L $<

clean:
	rm -f *.yang $(schemas) $(baty).rnc model.tree
