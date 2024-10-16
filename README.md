# Juniper

Juniper is a functional reactive programming language currently being developed by Caleb Helbling (currently at Purdue University). Louis Ades was a former developer at Tufts University.

The grammar in EBNF (extended Backus-Naur Form) is available in grammer.bnf

The purpose of Juniper is to provide a functional reactive programming platform for designing Arduino projects. FRP's high-level approach to timing-based events fits naturally with Arduino, with which programming almost entirely revolves around reacting to realtime events. Juniper transpiles to Arduino C++, which is then compiled to an Arduino executable.

### Language Features

- Compiler written in F#
- Transpiled to C++
- Statically typed
- Algebraic datatypes
- Use of records as datatypes (the equivalent of structs in C++)
- First class functions
- Type inference
- Use of templates to create maximum capacity length lists.

### Repository Contents

#### juniper.sln

- Microsoft Visual F# Solution used for developing this project

#### grammar.bnf

- Juniper Grammar and Concrete Syntax
- This is the grammar used for our language in this repository. It is represented in Extended Backus-Naur Form (EBNF), a metasyntax language used for the planning of Juniper's concrete syntax.

#### Juniper Subfolder

- Contains the code used for the compiler

## Building and Running the Juniper Compiler

To get started using Juniper, you'll first need to build the compiler using the source in this repository.

### For Windows machines:

#### For building:

1. If you do not have it already, download and install Microsoft Visual Studio 2022.
2. Clone this repository to your local machine.
3. Open /Juniper.sln in Visual Studio.
4. When the solution opens, go to the Solution Explorer and find the "References" tab underneath the "Juniper" project. Right click on "References", and select "Manage NuGet Packages." Verify that FParsec and QuikGraph are installed as part of your solution.
5. Build the Juniper project (either in Debug mode or Build mode).
6. The result should be a built .exe file called "Juniper.exe".
7. (Optional) Add Juniper.exe to PATH variables so that it can be run from any directory.

### For Linux machines:

Juniper is now on .NET 6! Juniper has been tested to work with Visual Studio Code, and probably works with Jetbrains Rider as well.

1. Install .NET 6 and Visual Studio Code on your Linux system
2. Install the Ionide extension for Visual Studio Code
3. Build using Ionide
4. Ensure that the `junstd/`, `cppstd/`, `examples/`, `wrappers/` and the `juniper` Linux run script are copied to the directory containing the build.

To build for Linux on Windows for distribution:

1. Enter the directory containing Juniper.sln from the terminal. Then run `dotnet publish -r linux-x64 --self-contained true`
2. Ensure that all the required directories and files got copied over into the built Linux folder. The folders to look for are: `junstd/`, `cppstd/`, `examples/`, `wrappers/` and the `juniper` Linux run script.
3. Move the build to a Linux system for testing/packaging.

### For MacOS machines:

1. Follow [these instructions](http://fsharp.org/use/mac/) (Option 3: Install Visual Studio for Mac) is tested, but choose the option that works best for you. The package manager is NuGet and has not been updated to paket.
2. Open the Juniper solution. Check the references, and refresh them if necessary. Verify that FParsec, Symbolism and QuikGraph work. Update Nuget packages.
3. Build the Juniper project.

### For writing Juniper files:

You can write Juniper files in any text editor, but the Visual Studio Code text editor supports a plugin for Juniper text highlighting, which is very useful for code readability. Install Visual Studio Code, and install the juniper language package from within the editor.

### For running the compiler:

1. From the command line, run "Juniper.exe -s [.jun modules as arguments] -o [output file]"
2. This should build your .cpp file. Compile and upload to your Arduino (with your preferred method, such as PlatformIO).


