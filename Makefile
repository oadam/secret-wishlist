EXECS=node_modules/.bin
OUT=bin

dev: $(OUT)/style.css $(OUT)/elm.js $(OUT)/index.dev.html $(OUT)/assets

prod: $(OUT)/style.min.css $(OUT)/elm.min.js $(OUT)/index.prod.html $(OUT)/assets

clean:
	rm -r $(OUT)

$(OUT)/assets:
	ln -s ../web/assets $@

$(OUT)/index.dev.html: web/index.html
	sed 's:CSS_HREF:style.css:; s:JS_HREF:elm.js:' $< > $@

$(OUT)/index.prod.html: web/index.html
	sed 's:CSS_HREF:style.min.css:; s:JS_HREF:elm.min.js:' $< > $@

$(OUT)/style.css: $(wildcard sass/*.scss)
	$(EXECS)/sass sass/main.scss $@

$(OUT)/style.min.css: $(OUT)/style.css
	$(EXECS)/csso --input-source-map $(OUT)/style.css.map --source-map file -i $< -o $@ --stat

$(OUT)/elm.js: $(wildcard elm/*.elm)
	$(EXECS)/elm make elm/Main.elm --output=$@

$(OUT)/elm.opti.js: $(wildcard elm/*.elm)
	$(EXECS)/elm make --optimize elm/Main.elm --output=$@

$(OUT)/elm.min.js: $(OUT)/elm.opti.js
	$(EXECS)/uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle > $@

serve:
	$(EXECS)/http-server $(OUT)

watch:
	while true; do \
		$(MAKE) --no-print-directory dev; \
		inotifywait -o /dev/null --exclude 'swp$$' -qre modify .; \
	done

.PHONY: dev prod watch clean
