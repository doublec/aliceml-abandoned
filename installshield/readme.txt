Creating a windows installer packages requires two things, namely

(a) a directory tree to be distributed (stored in the Files directory), and
(b) a InstallShield script (*.ism, stored in the Scripts directory).

To create the directory tree, including docs, use the uplevel Makefile.
Invoking "make" without arguments will print instructions for the required
steps.

Now start InstallShield by double-clicking on the Alice.ism.

First, go to "General Information/Summary Information Stream" and generate a
new Package Code. In case of a new release, also go to "General
Information/Product Properties" and generate new Product and Upgrade Codes.

Then start the release wizard via Build/Release Wizard. Follow the steps. Make sure that you

(a) use the existing configuration,
(b) choose Release 1,
(c) compress all files,
(d) create an installation launcher supporting both Win 9x and Win 2000/XP,
(e) extract engine from setup,
(f) do not cache installation on local machine,
(g) do not sign/password protect setup.exe,
(h) do not include or setup .NET framework,
(i) release settings say <ISProjectDataFolder> & Use long file names.

Build the release (this can take quite a while).

Once the installer package has been created, you will find it as shown under

   Scripts/Name/Name/Name Version/Release 1/DiskImages/DISK1/setup.exe

A note concering the setup scripts: Most of the file binding is dynamic,
that is, updated automatically if the contents of the associated directory
tree changes. This does not apply to key components, however. If you add new
key componentss, you will have to manually create new components entries in
the setup. Key components are required to perform assiciation of registry
entries, start menu entries and so on.

For example, the Alice installer contains two key files, namely
<Alice-Files>/bin/alice.exe and <Alice-Files>/doc/Alice.chm.

You will have to set up a new component if you add a new root directory,
otherwise dynamic file linking should take care of it. Go to
Advanced Views/Components and open the context menu for doing that.
Adding a new component does not automatically install it. To do so, go to
"Advanced Views/Setup Design", and right click to insert the component.
