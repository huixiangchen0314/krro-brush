.PHONY: clean compile test jar uberjar repl

VERSION := 0.1.0
JAR_FILE := target/krro-brush-$(VERSION).jar

all: test

clean:
	clj -T:build clean

jar: compile
	clj -T:build jar

repl:
	clj -M:dev

install: jar
	mvn install:install-file -Dfile=$(JAR_FILE) -DpomFile=pom.xml