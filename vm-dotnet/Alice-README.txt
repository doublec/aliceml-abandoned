This file describes how to install and run Alice for NGWS.

1. Unpack Alice.zip to a directory of your choice.
   Documentation is available as:

      Alice\doc\index.html

2. IMPORTANT: Adapt the installation path in vars.bat to the
   directory you unpacked Alice.zip into, and invoke vars.bat.

3. To run the precompiled demo do:

      stow streams.dll

   This demo is described in doc\examples.html.

4. You can invoke the compiler on the samples as follows:

      stoc examples\streams.aml streams.dll

   produces a streams.dll file in the current directory.

5. Invoke the application:

      stow streams.dll
