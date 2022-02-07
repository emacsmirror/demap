BUILD_DIR     = build
PROJECT       = demap
PROJECT_EL    = $(PROJECT).el
VERSION       = $(shell grep -soP -m 1 "(?<=^;; Version: ).*(?=$$)"          $(PROJECT_EL))
REQUIRES      = $(shell grep -soP -m 1 "(?<=^;; Package-Requires: ).*(?=$$)" $(PROJECT_EL))
OUT_FILE_NAME = $(PROJECT)-$(VERSION)
OUT_FILE      = $(BUILD_DIR)/$(OUT_FILE_NAME).tar
PKG_FILE      = $(BUILD_DIR)/$(PROJECT)-pkg.el
FILES         = $(PROJECT_EL) demap-minimap.el demap--tools.el demap-modes.el

all: $(OUT_FILE)

clean:
	rm -r $(BUILD_DIR)

$(PKG_FILE): $(PROJECT_EL)
	mkdir -p $(BUILD_DIR)
	echo "(define-package \""'$(PROJECT)'"\" \""'$(VERSION)'"\" \"\" '"'$(REQUIRES)'")" > $(PKG_FILE)

$(OUT_FILE): $(PKG_FILE) $(FILES)
	mkdir -p $(BUILD_DIR)
	tar -cf $(OUT_FILE)                   \
     --transform='s,^build/,,'            \
     --transform='s,^,$(OUT_FILE_NAME)/,' \
	 $(PKG_FILE)                          \
	 $(FILES)
