# asslangtool

A Tool for Software Tracing for MC680x0 processors.

This project has originally be started as a general Clojure library for MC680x0 assembly
listing files. Parsing them, printing them, analysing them. Software Tracing is just
one application of it.

## Usage

1. You need Java installed. It does not have to be the JDK. The standard JRE will be enough.
I tested it with Java 21. But other Java versions should als work. Please pay attention, that
some versions of Java are not free for professional use. In case of doubts, please use the
OpenJDK. These are alsways free, and normally part of your Linux distribution. The version,
that we currently use at work is https://jdk.java.net/java-se-ri/21.

2. Fetch Leiningen from https://leiningen.org/#install
You have to download the script lein.bat and add the installation path to your %PATH% environment.

3. Clone the project. If you don't have git installed, you can simply use the green button
Code -> "Download ZIP"

4. Navigate into the project folder

5. Execute
````
lein uberjar
````
You need internet connection. Leiningen will automatically fetch Clojure and a few libraries for you.

Now you get an executable jar file in target/asslangtool-0.1.0-SNAPSHOT-standalone.jar.

6. Execute the program with
````
java -jar target/asslangtool-0.1.0-SNAPSHOT-standalone.jar 127.0.0.1 7777 0x3000 0x3100 0x2 test.lis
````

where
* 127.0.0.1 is the IP address of the tracer
* 0x3000 is the starting address, where the listing is linked
* 0x3100 is the end address
* 0x2 is the program entry point (relatively from 0x3000)
* test.lis is a listing file that is automatically generated for testing purposes.

7. Test the program

Execute
````
nc -l -p 7777
````
In parallel execute
````
java -jar target/asslangtool-0.1.0-SNAPSHOT-standalone.jar 127.0.0.1 7777 0x3000 0x3100 0x2 test.lis
````
Now you can type the tracing into the nc window. For example
````
> TRACE FROM 12288 TO 12544
< 3010 3030
< 3040 
< DONE
````
After each instruction you can open the file test.lis and see how the coverage progresses.

## License

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.