# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

BUILD_DIR     = build
PROJECT       = demap
PROJECT_EL    = $(PROJECT).el
VERSION       = $(shell grep -soP -m 1 "(?<=^;; Version: ).*(?=$$)"          $(PROJECT_EL))
REQUIRES      = $(shell grep -soP -m 1 "(?<=^;; Package-Requires: ).*(?=$$)" $(PROJECT_EL))
OUT_FILE_NAME = $(PROJECT)-$(VERSION)
OUT_FILE      = $(BUILD_DIR)/$(OUT_FILE_NAME).tar
PKG_FILE      = $(BUILD_DIR)/$(PROJECT)-pkg.el
FILES         = $(PROJECT_EL) demap-minimap.el demap-tools.el demap-modes.el

all: $(OUT_FILE)

clean:
	rm -r $(BUILD_DIR)

test:
	emacs -batch                                        \
		--eval "(package-refresh-contents)"             \
		--eval "(package-install-file \"$(OUT_FILE)\")" \
		--eval "(demap-open)"

$(PKG_FILE): $(PROJECT_EL)
	mkdir -p $(BUILD_DIR)
	echo "(define-package \""'$(PROJECT)'"\" \""'$(VERSION)'"\" \"\" '"'$(REQUIRES)'")" > $(PKG_FILE)

$(OUT_FILE): $(PKG_FILE) $(FILES)
	mkdir -p $(BUILD_DIR)
	tar -cf $(OUT_FILE)                      \
		--transform='s,^$(BUILD_DIR)/,,'     \
		--transform='s,^,$(OUT_FILE_NAME)/,' \
		$(PKG_FILE)                          \
		$(FILES)
