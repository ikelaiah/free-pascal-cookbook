# Setup Lazarus IDE and FPC

Here are two easy ways to set up Lazarus IDE and Free Pascal Compiler (FPC) on your system.

- [Use Lazarus IDE's Installer (easiest)](#use-lazarus-ides-installer-easiest)
- [Use `fpcupdeluxe` (more options)](#use-fpcupdeluxe-more-options)

## Use Lazarus IDE's Installer (easiest)

1. Head to [https://www.lazarus-ide.org](https://www.lazarus-ide.org).

2. Click the **Download** button to get an installer for your OS.

![install-lazarus](../../assets/install-lazarus-fpc.png)

## Use `fpcupdeluxe` (more options)

For most new users, the official Lazarus IDE installer (mentioned above) is the recommended and simplest way to get started.

However, if you find you need more control over specific FPC and Lazarus versions (like using the very latest development builds, older versions, or setting up for cross-compilation), `fpcupdeluxe` is a powerful alternative. It's generally more suited for users who have specific requirements or are comfortable with more advanced setup options.

### Install on Windows

#### Get `fpcupdeluxe`

1. Head to [https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases).
2. Download the latest `fpcupdeluxe-i386-win32.exe` or `fpcupdeluxe-x86_64-win64.exe` if you prefer a 64-bit Lazarus IDE.
3. Run the downloaded `fpcupdeluxe` executable.

#### Install FPC and Lazarus

Steps:

1. **Set Installation Directory**: In `fpcupdeluxe`, the first important step is to specify an **Install dir**. This is where FPC and Lazarus will be installed (e.g., `C:\Lazarus_fpcupdeluxe`). Make sure this path does not contain spaces.
2. **Select Versions**:
    - From the **FPC compiler** dropdown, choose a version. For stability, `stable` or the latest `fixes (x.x.x)` version is recommended.
    - From the **Lazarus IDE** dropdown, choose a version. The latest `release (x.x.x)` is usually a good choice.
3. **Install**: Click the large **Install/update FPC + Lazarus** button.
4. **Confirm**: Click **Yes** when the confirmation box appears.

![Step1](../../assets/fcpupdeluxe-step-01.png)

The installation of FPC and Lazarus might take a while. Sit back and relax.

Once successfully installed, the console will let you know a shortcut to Lazarus IDE is available on the Windows desktop.

```
SUCCESS: installation by fpcupdeluxe complete !

Fpcupdeluxe has created a desktop shortcut to start Lazarus.
Shortcut-name: Lazarus_fpcupdeluxe
Lazarus by fpcupdeluxe MUST be started with this shortcut !!
```

![Step1-end](../../assets/fcpupdeluxe-step-01-end.png)

#### Add recommended modules

There are two more recommended modules to install.

- With the **Online Package Manager (OPM)**, you can easily install online packages right within the Lazarus IDE.
- The **Dock (anchordocking)** module turns the Lazarus IDE into a one-window setup.

Steps:

1. Click on the **Modules** tab.
2. Click on the **OPM** button. Click yes when the confirmation box appears. Wait until the installation is completed before moving to the last step.
3. Click on the **Dock** button. Click yes when the confirmation box appears.

![Step1-end](../../assets/fcpupdeluxe-step-02.png)

Once the installation is completed successfully, close `fpcupdeluxe`.

#### Launch Lazarus IDE

At the end of the installation, you will find `Lazarus_fcpupdeluxe` on your desktop.

Double click to run it. You will see Lazarus IDE as shown below.

![LazarusIDE](../../assets/lazarus-ide-start-01.png)

### Install on Linux or macOS

For Linux and macOS, `fpcupdeluxe` is typically used as a command-line tool, though precompiled GUI versions might be available for some systems.

Consult the official [`fpcupdeluxe` GitHub page](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) for detailed instructions. Look for sections relevant to your operating system, including how to download or build the tool and the commands for installing FPC and Lazarus.


