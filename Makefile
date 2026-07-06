.PHONY: clean compile test jar uberjar repl

VERSION := 0.1.0
JAR_FILE := target/homunculus-$(VERSION).jar

all: test

clean:
	clj -T:build clean

jar: compile
	clj -T:build jar

repl:
	clj -M:dev

install: jar
	mvn install:install-file -Dfile=target/krro-brush-0.1.0.jar -DpomFile=pom.xml