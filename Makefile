
FILES         = demap-pkg.el demap.el demap-minimap.el demap--tools.el demap-track-w.el
BUILD_DIR     = build
VERSION       = $(shell grep -soP -m 1 "(?<=\(define-package \"demap\" \").*(?=\"\))" demap-pkg.el)
OUT_FILE_NAME = demap-$(VERSION)
OUT_FILE      = $(BUILD_DIR)/$(OUT_FILE_NAME).tar


$(OUT-FILE): $(FILES)
	mkdir -p $(BUILD_DIR)
	tar -cf $(OUT-FILE)                  \
    --transform='s,^,$(OUT_FILE_NAME)/,' \
    ${FILES}
