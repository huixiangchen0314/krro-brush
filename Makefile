.PHONY: clean test jar uberjar install repl

clean:
	clojure -T:build clean

test:
	clojure -M:test

jar:
	clojure -T:build jar

uberjar:
	clojure -T:build uberjar

install: jar
	mvn install:install-file -Dfile=target/krro-brush-0.1.0.jar -DpomFile=pom.xml

repl:
	clojure -M:repl