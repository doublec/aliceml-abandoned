This file describes how to install and run Alice for NGWS.

1. Unpack Alice.zip to a directory of your choice.
   Documentation is available as:

      Alice\doc\index.html

2. Adapt the installation path in vars.bat to the directory
   you unpacked Alice.zip into, and invoke vars.bat.

3. To run the precompiled demo do:

      stow streams.dll

   This demo is described in doc\examples.html.

4. You can invoke the compiler on the samples as follows:

      stoc examples\streams.aml streams.il

   produces a streams.il file in the current directory.
   Now invoke ilasm, provided with the NGWS SDK, to generate
   streams.dll:

      ilasm /dll streams.il

5. IMPORTANT - Due to a strange bug in the system it is
   necessary to reassemble some base components after each
   time you performed step 4, as follows:

      ilasm /dll TextIO.il
      ilasm /dll Tools.il

6. Invoke the application:

      stow streams.dll
