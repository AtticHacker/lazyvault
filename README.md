IMPORTANT: If you're looking for a sandboxing tool that makes it easier to install packages without dependency hell, Then you are probably looking for cabal-dev.
This tool was made for personal gain because I wanted a quick way to swap sandboxes globally. The code is very old despite the upload date and I haven't bothered refactoring it.
It's only been used on GNU/Linux operating systems.

# LazyVault

LazyVault is a simple sandboxing tool for managing your cabal packages.

# Note

At the moment LazyVault is only supported under Unix / Gnu systems and has only been tested by me on Gnu/Linux.

# About

LazyVault lets you create new global cabal sandboxes on your system. Instead of creating a sandbox inside your project, a new sandbox is created in the LazyVault directory for global use. You can switch between sandboxes, create, delete and store your existing cabal environment.

# Installation

For a clean start I recommend you delete (or move) your current .cabal and .ghc directories and install LazyVault in a fesh .cabal dir.

    # cabal install lazyvault

You have now installed LazyVault in your current cabal directory, make sure you add $HOME/.cabal/bin to your $PATH in order to access the executable.

# Usage

Once you're done, store your currently used cabal / ghc directories with the following command

    #LazyVault store LazyVault

This will store your current cabal environment as a sandbox and name it LazyVault.

Add this piece of code to your .bashrc / .bashprofile / .zshrc to include all the bin directories to your $PATH

    source $HOME/.lazyVault/binPaths

You can now access the LazyVault executable even though it's stored as a sandbox.

### To list your sanboxes issue this command:

    # LazyVault list
    Available sandboxes:
    none **
    LazyVault

Here you see that no sandbox is selected, but LazyVault is available. We can leave the LazyVault sandbox as it is, since we only need to access the binary. Let's install the `random` package in a new sandbox.

### Create a sandbox:

    LazyVault create Random

Check your sandbox list again:

    # LazyVault list
    Available sandboxes:
    none **
    Random
    LazyVault

Random has been added, but none is still selected. Let's set Random as our current cabal environment:

    # LazyVault set Random

Check the list again:

    # LazyVault list
    Random **
    LazyVault

As we can see, Random is now selected. We can now proceed in installing the random package.

    # cabal update ; cabal install random

`random` has now been installed in the `Random` sandbox, and is accessible globally if Random is set.
