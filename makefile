#@IgnoreInspection BashAddShebang
INSTALL = /usr/bin/install -c
CWD = `pwd`
REBAR = $(CWD)/rebar

# 发布路径
RELEASEDIR = release


BEAMDIR = $(RELEASEDIR)/ebin

INCLUDEDIR = $(RELEASEDIR)/include

PRIVDIR = $(RELEASEDIR)/priv

PBINDIR = $(RELEASEDIR)/bin

TPLDIR = $(PRIVDIR)/templates

DOCROOTDIR = $(PRIVDIR)/docroot


all: deps src
install: all
	$(INSTALL) -d $(BEAMDIR)
	$(INSTALL) -m 644 ebin/*.app $(BEAMDIR)
	$(INSTALL) -m 644 ebin/*.beam $(BEAMDIR)
	$(INSTALL) -m 644 deps/*/ebin/*.app $(BEAMDIR)
	$(INSTALL) -m 644 deps/*/ebin/*.beam $(BEAMDIR)
	#
	# header files
	$(INSTALL) -d $(INCLUDEDIR)
	$(INSTALL) -m 644 include/*.hrl $(INCLUDEDIR)
	$(INSTALL) -m 644 deps/*/include/*.hrl $(INCLUDEDIR)
	#
	#
	# webservice files
	$(INSTALL) -d $(TPLDIR)
	$(INSTALL) -d $(DOCROOTDIR)
	$(INSTALL) -m 644 priv/templates/* $(TPLDIR)
	cp -r priv/docroot $(DOCROOTDIR)/..
	chmod -R 777 $(DOCROOTDIR)


deps: deps/.get

deps/.get:
	rm -rf deps/.get
	rm -rf deps/.built
	$(REBAR) get-deps && :> deps/.get

deps/.built: deps/.get
	$(REBAR) compile && :> deps/.built

src: deps/.built
	$(REBAR) skip_deps=true compile

clean:
	rm -rf deps/.got
	rm -rf deps/.built
	rm -rf test/*.beam
	$(REBAR) clean
