# -*- mode: makefile-gmake; -*-

all: prod

dev:
	BOOT_JVM_OPTIONS="--add-modules java.xml.bind" boot dev

prod:
	boot prod

.PHONY: dev prod
