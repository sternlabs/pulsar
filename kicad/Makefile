BOARD= pulsar

GTAG= F_Cu.gtl B_Cu.gbl F_Mask.gts B_Mask.gbs F_SilkS.gto B_SilkS.gbo Edge_Cuts.gko In1_Cu.g2l In2_Cu.g3l F_Paste.gtp B_Paste.gbp

VER?= $(shell git describe --always)
ZIPFILE?= ${BOARD}-${VER}.zip
STENCIL_ZIPFILE?= ${BOARD}-${VER}-stencil.zip

COPYFILE=cp $^ $@

%.gko: %.gbr
	${COPYFILE}

%.g2l: %.gbr
	${COPYFILE}

%.g3l: %.gbr
	${COPYFILE}

%.xln: %.drl
	${COPYFILE}

zip: ${ZIPFILE}
#${STENCIL_ZIPFILE}

${ZIPFILE}: $(addprefix ${BOARD}-,${GTAG}) ${BOARD}.xln
	-rm -f $@
	zip $@ $^

# ${STENCIL_ZIPFILE}: $(addprefix ${BOARD}-,Edge_Cuts.gbr F_Paste.

clean:
	-rm -f ${ZIPFILE}
