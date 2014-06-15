PKGNAME=erlnntp
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`
LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
# get application vsn from app file
VERSION=$(shell erl -pa ebin/ -eval 'application:load(${PKGNAME}), {ok, Vsn} = application:get_key(${PKGNAME}, vsn), io:format("~s~n", [Vsn])' -s init stop -noshell)

all: src

src: FORCE
	@erl -make
	@cp src/${PKGNAME}.app.src ebin/${PKGNAME}.app

clean:
	rm -rf erl_crash.dump *.boot *.rel *.script ebin/*.beam ebin/emongo.app

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Emakefile Makefile priv README.markdown src t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install: src
	@mkdir -p $(LIBDIR)/$(PKGNAME)-$(VERSION)/ebin
	@mkdir -p $(LIBDIR)/$(PKGNAME)-$(VERSION)/include
	@mkdir -p $(LIBDIR)/$(PKGNAME)-$(VERSION)/priv
	for i in ebin/*.beam include/*.hrl ebin/*.app priv/*; do install $$i $(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
	if [ -d "/etc/init.d/" ]; then ln -s $(LIBDIR)/$(PKGNAME)-$(VERSION)/priv/erlnntp /etc/init.d/erlnntp; fi	

shell: src
	erl -pa ebin

FORCE:
